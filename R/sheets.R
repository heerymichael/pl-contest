# Google Sheets data access layer
library(googlesheets4)
library(googledrive)
library(dplyr)
library(lubridate)
library(tibble)
library(uuid)

message(">>> SOURCED R/sheets.R at ", format(Sys.time()))

gs_auth <- function() {
  gs4_auth(path = Sys.getenv("GS_AUTH_PATH"))
  drive_auth(path = Sys.getenv("GS_AUTH_PATH"))
}

sheet_id <- function() Sys.getenv("GS_SHEET_ID")

read_sheet_tab <- function(tab) {
  read_sheet(sheet_id(), sheet = tab)
}

append_sheet_row <- function(tab, row_data) {
  sheet_append(sheet_id(), data = row_data, sheet = tab)
}

get_contest_config <- function() {
  raw <- read_sheet_tab("contest_config")
  as.list(raw[1, ])
}

# is_locked() lives in R/lock.R now (uses fresh-read on every call;
# protected against stale cache).

get_players <- function() {
  read_sheet_tab("players") |> filter(is_active == TRUE)
}

get_teams <- function() {
  read_sheet_tab("teams")
}

get_entries_for_user <- function(user_email) {
  all <- read_sheet_tab("entries")
  if (nrow(all) == 0) return(all)
  all |> filter(user_email == !!user_email)
}

count_entries_for_user <- function(user_email) {
  nrow(get_entries_for_user(user_email))
}

# All entries, for the leaderboard. Fresh read — the board should
# reflect new submissions on each visit. Returns the entries tab as-is
# (entry_id, user_email, entry_name, created_at, updated_at, locked_at,
# display_name, ...); callers do their own sorting/scoring.
get_all_entries <- function() {
  message(">>> get_all_entries")
  all <- read_sheet_tab("entries")
  message("    get_all_entries read ", nrow(all), " rows")
  all
}

# All roster picks across all entries, for the leaderboard (per-entry
# roster sizes now; live-player counts and scoring later). Fresh read.
get_all_roster_picks <- function() {
  message(">>> get_all_roster_picks")
  all <- read_sheet_tab("roster_picks")
  message("    get_all_roster_picks read ", nrow(all), " rows")
  all
}

log_action <- function(user_email, action, details = list()) {
  all <- tryCatch(read_sheet_tab("audit_log"), error = function(e) tibble())
  new_id <- if (nrow(all) == 0) 1L else max(all$event_id, na.rm = TRUE) + 1L
  
  row <- tibble(
    event_id   = new_id,
    user_email = user_email,
    action     = action,
    details    = jsonlite::toJSON(details, auto_unbox = TRUE) |> as.character(),
    created_at = Sys.time()
  )
  append_sheet_row("audit_log", row)
}

# --- Entry creation -----------------------------------------------------

# IMPORTANT: locked_at is intentionally left NA at creation. Per the
# agreed lock-time design, the column is treated as informational only.
# Lock state is derived from contest_config.lock_at + Sys.time(); the
# per-entry column is never populated.
#
# display_name is denormalised onto the entry row at creation so the
# leaderboard (and Phase 3 scoring) can render from the Sheet alone,
# without a join back to the users SQLite DB. Column order in the
# tibble must match the sheet (entry_id .. locked_at, display_name) —
# sheet_append writes positionally, not by header name.
create_entry <- function(user_email, entry_name, display_name) {
  message(">>> create_entry user=", user_email, " name=", entry_name,
          " display=", display_name)
  new_id <- UUIDgenerate()
  now    <- Sys.time()
  
  row <- tibble(
    entry_id     = new_id,
    user_email   = user_email,
    entry_name   = entry_name,
    created_at   = now,
    updated_at   = now,
    locked_at    = NA,   # informational only — see comment above
    display_name = display_name
  )
  append_sheet_row("entries", row)
  message("    create_entry wrote entry_id=", new_id)
  new_id
}

save_roster_picks <- function(entry_id, picks) {
  message(">>> save_roster_picks entry_id=", entry_id, " n=", nrow(picks))
  now <- Sys.time()
  
  rows <- picks |>
    mutate(
      pick_id    = vapply(seq_len(nrow(picks)), function(i) UUIDgenerate(),
                          character(1)),
      entry_id   = entry_id,
      slot       = position,
      created_at = now
    ) |>
    select(pick_id, entry_id, player_id, slot, created_at)
  
  append_sheet_row("roster_picks", rows)
  message("    save_roster_picks wrote ", nrow(rows), " picks")
  invisible(rows)
}

# --- Entry editing ------------------------------------------------------

# Return the picks for a single entry, joined with player info.
# Output columns: player_id, name, team, position.
get_picks_for_entry <- function(entry_id) {
  message(">>> get_picks_for_entry entry_id=", entry_id)
  all_picks <- read_sheet_tab("roster_picks")
  entry_picks <- all_picks |> filter(entry_id == !!entry_id)
  message("    found ", nrow(entry_picks), " picks")
  if (nrow(entry_picks) == 0) {
    return(tibble(player_id = integer(0), name = character(0),
                  team = character(0), position = character(0)))
  }
  
  players <- get_players()
  entry_picks |>
    select(player_id) |>
    left_join(players |> select(player_id, name, team, position),
              by = "player_id")
}

# Wipe existing picks for an entry, write the new set, and bump
# updated_at on the entries row. Uses range_write() for the entry row
# (requires read-then-write to find the row number).
#
# `picks` is a tibble with columns player_id and position.
update_entry_picks <- function(entry_id, picks) {
  message(">>> update_entry_picks entry_id=", entry_id, " n=", nrow(picks))
  now <- Sys.time()
  
  # 1. Delete existing roster_picks rows for this entry.
  # googlesheets4 has no native delete-by-filter; we rewrite the whole
  # roster_picks tab with the chosen entry's rows removed, then append
  # the new picks. For our scale (< 1000 picks total) this is fine.
  all_picks <- read_sheet_tab("roster_picks")
  remaining <- all_picks |> filter(entry_id != !!entry_id)
  message("    removing ", nrow(all_picks) - nrow(remaining),
          " existing picks; ", nrow(remaining), " others retained")
  
  write_sheet(remaining, ss = sheet_id(), sheet = "roster_picks")
  
  # 2. Append new picks.
  save_roster_picks(entry_id, picks)
  
  # 3. Bump updated_at on the entries row.
  entries <- read_sheet_tab("entries")
  row_idx <- which(entries$entry_id == entry_id)
  if (length(row_idx) != 1) {
    stop("update_entry_picks: expected exactly one matching entry row, found ",
         length(row_idx))
  }
  # +1 for header row; range_write is 1-indexed and the header is row 1
  sheet_row <- row_idx + 1L
  
  # updated_at is column 5 (entry_id, user_email, entry_name, created_at,
  # updated_at, locked_at, display_name)
  range_write(
    ss    = sheet_id(),
    data  = tibble(updated_at = now),
    sheet = "entries",
    range = paste0("E", sheet_row),
    col_names = FALSE
  )
  message("    bumped updated_at for entry_id=", entry_id,
          " at row ", sheet_row)
  
  invisible(NULL)
}


# --- Match stats (Phase 3 scoring) --------------------------------------

# All player_match_stats rows, fresh read. Tolerates an empty/missing tab
# (returns a zero-row tibble) so the leaderboard renders pre-scoring
# em-dashes instead of erroring. Populated by ingest_match_stats.R;
# cached via get_match_stats_cached() in R/cache.R.
get_player_match_stats <- function() {
  message(">>> get_player_match_stats")
  out <- tryCatch(read_sheet_tab("player_match_stats"),
                  error = function(e) {
                    message("    get_player_match_stats read FAILED: ",
                            conditionMessage(e))
                    NULL
                  })
  if (is.null(out) || nrow(out) == 0) {
    message("    get_player_match_stats: no rows")
    return(tibble::tibble(match_id = character(0), player_id = character(0)))
  }
  message("    get_player_match_stats read ", nrow(out), " rows across ",
          length(unique(out$match_id)), " matches")
  out
}