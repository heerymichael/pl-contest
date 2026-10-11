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
  players <- read_sheet_tab("players")
  # PL players tab carries no is_active column (all pool players are
  # active); synthesise it, mirroring snapshot_pl_reference.R, so the
  # WC-era filter is a no-op instead of a crash on live reads.
  if (!"is_active" %in% names(players)) players$is_active <- TRUE
  players |> filter(is_active == TRUE)
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
#
# PERIOD-AWARE: roster_picks carries a full 15-row roster per period
# (period 1 = GW1-2 as locked; period 2 = GW3 onward, present only for
# entries touched by a club-clash auto-sub or a window transfer). This
# returns the entry's HIGHEST present period — the roster they hold now
# and the one the editor should load. Cast before comparing: Sheets
# hands period back as double (1.0).
#
# The picks themselves are a live read (roster_picks is writable state).
# The player join is served from the local RDS snapshot via
# get_players_all_local() — players is static reference data, so reading
# the 665-row tab from Sheets on every call was a second full round-trip
# on the critical path with nothing to show for it. Using the *_all_local
# variant (not the active-only one) also means a pick pointing at a
# deactivated player still resolves to a name instead of going NA.
get_picks_for_entry <- function(entry_id) {
  message(">>> get_picks_for_entry entry_id=", entry_id)
  all_picks <- read_sheet_tab("roster_picks")
  entry_picks <- all_picks |> filter(entry_id == !!entry_id)
  message("    found ", nrow(entry_picks), " picks across all periods")
  if (nrow(entry_picks) == 0) {
    return(tibble(player_id = integer(0), name = character(0),
                  team = character(0), position = character(0)))
  }
  
  if ("period" %in% names(entry_picks)) {
    pp <- suppressWarnings(as.integer(entry_picks$period))
    if (any(!is.na(pp))) {
      top <- max(pp, na.rm = TRUE)
      entry_picks <- entry_picks[!is.na(pp) & pp == top, , drop = FALSE]
      message("    using period ", top, " — ", nrow(entry_picks), " picks")
    }
  }
  
  players <- get_players_all_local()
  entry_picks |>
    # read_sheet() returns player_id as double; the snapshot stores it as
    # integer. Coerce before the join so the key types match exactly.
    mutate(player_id = as.integer(player_id)) |>
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
  
  # HARD STOP POST-LOCK. This function rewrites the whole roster_picks tab
  # minus the entry's rows — every period. Once period-2 rosters exist
  # (club-clash auto-subs, window transfers) an unscoped rewrite would
  # silently destroy GW1-2 history. Post-lock roster changes go through
  # apply_transfers() below, which never rewrites the tab.
  if (is_locked()) {
    stop("update_entry_picks: contest is locked — post-lock roster changes ",
         "must go through apply_transfers()", call. = FALSE)
  }
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


# --- Transfer window write path -------------------------------------------

# Contiguous sheet-row blocks (1-indexed, header = row 1) occupied by the
# given 0/1-indexed data rows. Used to overwrite or delete an entry's rows
# in place without ever rewriting the tab.
.sheet_row_blocks <- function(data_rows) {
  if (length(data_rows) == 0) return(list())
  r <- sort(data_rows) + 1L                     # +1 for the header row
  breaks <- c(0L, which(diff(r) != 1L), length(r))
  lapply(seq_len(length(breaks) - 1L), function(i) {
    seg <- r[(breaks[i] + 1L):breaks[i + 1L]]
    c(first = seg[1], last = seg[length(seg)])
  })
}

# The roster an entry held at the moment the window opened: its period-1
# roster with any club-clash auto-subs (and any other non-window
# correction effective from the transfer gameweek) applied. Window
# transfers are measured against THIS, however many times the entry
# saves, so the budget is cumulative and an A->B->A across two saves
# costs nothing. Returns a character vector of player_ids.
transfer_baseline <- function(entry_id, picks, transfers, effective_gw) {
  eid <- as.character(entry_id)
  rows <- picks[as.character(picks$entry_id) == eid, , drop = FALSE]
  pp   <- suppressWarnings(as.integer(rows$period))
  base <- as.character(rows$player_id[!is.na(pp) & pp == 1L])
  if (length(base) == 0) {
    # No period tag at all — the only roster there is.
    base <- as.character(rows$player_id)
  }
  
  fixes <- transfers[
    as.character(transfers$entry_id) == eid &
      tolower(trimws(transfers$type)) != "window" &
      suppressWarnings(as.integer(transfers$effective_gw)) == effective_gw,
    , drop = FALSE]
  fixes <- fixes[order(as.character(fixes$created_at)), , drop = FALSE]
  for (i in seq_len(nrow(fixes))) {
    out_id <- as.character(fixes$out_player_id[i])
    in_id  <- as.character(fixes$in_player_id[i])
    k <- which(base == out_id)
    if (length(k) == 1L) base[k] <- in_id
  }
  base
}

# Roster-shape + one-per-club check, independent of the picker. `ids` are
# player_ids (character); `players` the full players table. Returns NULL
# if valid, else a one-line reason.
.roster_problem <- function(ids, players, cfg) {
  if (anyDuplicated(ids)) return("duplicate player")
  pl <- players[match(as.integer(ids), as.integer(players$player_id)), ,
                drop = FALSE]
  if (any(is.na(pl$player_id))) return("unknown player_id")
  n <- as.integer(cfg$roster_total %||% 15L)
  if (nrow(pl) != n) return(paste0("roster has ", nrow(pl), " players, needs ", n))
  want <- c(GK  = as.integer(cfg$max_gk  %||% 2L),
            DEF = as.integer(cfg$max_def %||% 5L),
            MID = as.integer(cfg$max_mid %||% 5L),
            FWD = as.integer(cfg$max_fwd %||% 3L))
  got <- table(factor(pl$position, levels = names(want)))
  if (!identical(as.integer(got), unname(want))) {
    return(paste0("positions ", paste(names(got), as.integer(got),
                                      collapse = " "), " — need ",
                  paste(names(want), want, collapse = " ")))
  }
  if (anyDuplicated(pl$team)) {
    return(paste0("two players at ",
                  paste(unique(pl$team[duplicated(pl$team)]), collapse = ", ")))
  }
  NULL
}

# Apply an entry's transfer-window changes. `new_picks` is a tibble with
# player_id (and position, ignored — re-derived server-side).
#
# Writes, in order, and never rewrites a tab:
#   1. roster_picks — 15 rows tagged with the target period. If the entry
#      already has a period-N block (auto-sub, or an earlier save this
#      window) the new rows are range_write()n OVER that block in place;
#      otherwise they are appended. Period-1 rows are never touched.
#   2. transfers — the entry's existing `window` rows are deleted (per
#      contiguous block, bottom-up) and one `window` row per NET swap vs.
#      the window-open baseline is appended. The log therefore always
#      shows the entry's final position, not its edit history.
#   3. entries.updated_at bumped.
#
# Hard stops (before any write): window not open; roster fails shape /
# one-per-club; any incoming player inactive; swaps exceed transfers_max.
#
# Returns list(ok, n_used, n_max, out_ids, in_ids) for the UI.
apply_transfers <- function(entry_id, new_picks) {
  message(">>> apply_transfers entry_id=", entry_id, " n=", nrow(new_picks))
  eid <- as.character(entry_id)
  
  # 1. Fresh window check — the only gate that matters.
  state <- transfer_window_state()
  if (!identical(state, "open")) {
    stop("The transfer window is ", state, ". No changes can be saved.",
         call. = FALSE)
  }
  cfg      <- get_contest_config()
  n_max    <- as.integer(cfg$transfers_max %||% 2L)
  eff_gw   <- as.integer(cfg$transfer_effective_gw %||% 3L)
  period   <- transfer_target_period(cfg)
  now      <- Sys.time()
  
  # 2. Fresh reads — character-typed so ids and periods survive intact.
  picks <- read_sheet(sheet_id(), sheet = "roster_picks", col_types = "c")
  trs   <- get_all_transfers()
  players <- get_players_all_local()
  message("    apply_transfers read ", nrow(picks), " picks, ", nrow(trs),
          " transfers; target period ", period, ", max ", n_max)
  
  new_ids <- as.character(as.integer(new_picks$player_id))
  
  # 3. Roster validity, server-side.
  problem <- .roster_problem(new_ids, players, cfg)
  if (!is.null(problem)) {
    stop("Roster invalid: ", problem, call. = FALSE)
  }
  
  # 4. Delta vs the window-open baseline.
  base    <- transfer_baseline(eid, picks, trs, eff_gw)
  out_ids <- setdiff(base, new_ids)
  in_ids  <- setdiff(new_ids, base)
  if (length(out_ids) != length(in_ids)) {
    stop("Roster size changed (", length(out_ids), " out, ", length(in_ids),
         " in)", call. = FALSE)
  }
  n_used <- length(in_ids)
  message("    delta vs baseline: ", n_used, " swap(s) — out [",
          paste(out_ids, collapse = ","), "] in [",
          paste(in_ids, collapse = ","), "]")
  if (n_used > n_max) {
    stop("That would use ", n_used, " transfers; the limit is ", n_max, ".",
         call. = FALSE)
  }
  incoming <- players[match(as.integer(in_ids), as.integer(players$player_id)), ,
                      drop = FALSE]
  inactive_in <- incoming$name[!(incoming$is_active %in% c(TRUE, "TRUE", 1))]
  if (length(inactive_in) > 0) {
    stop("Not in the pool: ", paste(inactive_in, collapse = ", "),
         call. = FALSE)
  }
  
  # 5. roster_picks — overwrite the existing period block or append.
  pos_of <- players$position[match(as.integer(new_ids),
                                   as.integer(players$player_id))]
  rows <- tibble(
    pick_id    = vapply(seq_along(new_ids), function(i) UUIDgenerate(),
                        character(1)),
    entry_id   = eid,
    player_id  = as.integer(new_ids),
    slot       = pos_of,
    created_at = now,
    period     = as.integer(period)
  )
  
  pp <- suppressWarnings(as.integer(picks$period))
  existing_idx <- which(as.character(picks$entry_id) == eid &
                          !is.na(pp) & pp == period)
  if (length(existing_idx) == 0) {
    sheet_append(sheet_id(), rows, sheet = "roster_picks")
    message("    appended ", nrow(rows), " period-", period, " rows")
  } else {
    blocks <- .sheet_row_blocks(existing_idx)
    if (length(blocks) != 1L || length(existing_idx) != nrow(rows)) {
      stop("apply_transfers: entry's period-", period, " rows are not one ",
           "contiguous block of ", nrow(rows), " (found ",
           length(existing_idx), " rows in ", length(blocks),
           " block(s)) — refusing to write. Inspect roster_picks.",
           call. = FALSE)
    }
    b <- blocks[[1]]
    rng <- paste0("A", b[["first"]], ":F", b[["last"]])
    range_write(sheet_id(), data = rows, sheet = "roster_picks",
                range = rng, col_names = FALSE, reformat = FALSE)
    message("    overwrote period-", period, " block ", rng, " in place")
  }
  
  # 6. transfers — drop the entry's previous window rows, append net delta.
  old_win <- which(as.character(trs$entry_id) == eid &
                     tolower(trimws(trs$type)) == "window")
  if (length(old_win) > 0) {
    blocks <- .sheet_row_blocks(old_win)
    for (b in rev(blocks)) {           # bottom-up so indices stay valid
      range_delete(sheet_id(), sheet = "transfers",
                   range = cell_rows(c(b[["first"]], b[["last"]])))
    }
    message("    removed ", length(old_win), " previous window row(s) in ",
            length(blocks), " block(s)")
  }
  if (n_used > 0) {
    tr_rows <- tibble(
      transfer_id   = vapply(seq_len(n_used), function(i) UUIDgenerate(),
                             character(1)),
      entry_id      = eid,
      out_player_id = out_ids,
      in_player_id  = in_ids,
      type          = "window",
      effective_gw  = eff_gw,
      note          = "",
      created_at    = format(lubridate::with_tz(now, "Europe/London"),
                             "%Y-%m-%d %H:%M:%S")
    )
    sheet_append(sheet_id(), tr_rows, sheet = "transfers")
    message("    appended ", nrow(tr_rows), " window transfer row(s)")
  }
  
  # 7. entries.updated_at (same pattern as update_entry_picks).
  entries <- read_sheet_tab("entries")
  row_idx <- which(entries$entry_id == eid)
  if (length(row_idx) == 1L) {
    range_write(sheet_id(), data = tibble(updated_at = now), sheet = "entries",
                range = paste0("E", row_idx + 1L), col_names = FALSE)
    message("    bumped updated_at for entry_id=", eid)
  } else {
    message("    !! could not bump updated_at — ", length(row_idx),
            " matching entry rows")
  }
  
  message("<<< apply_transfers done — ", n_used, " of ", n_max, " used")
  list(ok = TRUE, n_used = n_used, n_max = n_max,
       out_ids = out_ids, in_ids = in_ids)
}

# --- Match stats (Phase 3 scoring) --------------------------------------

# Columns on player_match_stats that must be numeric downstream. The tab is
# read as all-character (below) and these are coerced explicitly.
PMS_NUMERIC_COLS <- c(
  "matchday", "minutes_played", "goals", "assists", "total_shots",
  "shots_on_target", "tackles", "interceptions", "saves", "penalty_saves",
  "own_goals", "yellow_cards", "red_cards", "team_goals_for",
  "team_goals_against"
)

# All player_match_stats rows, fresh read. Tolerates an empty/missing tab
# (returns a zero-row tibble) so the leaderboard renders pre-scoring
# em-dashes instead of erroring. Populated by ingest_match_stats.R;
# cached via get_match_stats_cached() in R/cache.R.
#
# READ AS CHARACTER, THEN COERCE EXPLICITLY. The ingest writes every cell as
# text so ids survive intact (api_player_id values like "pl_02935210" carry
# leading zeros that a numeric guess would destroy). Letting read_sheet()
# type-guess would make the column types depend on what happens to be in the
# tab on a given day — fine in July, wrong the first time a column is all
# zeros or all blank. So the parse is pinned here and the numeric columns are
# named explicitly: R/scoring.R coerces defensively via .num0(), but
# R/leaderboard.R sums these columns directly and would fail on character.
get_player_match_stats <- function() {
  message(">>> get_player_match_stats")
  out <- tryCatch(read_sheet(sheet_id(), sheet = "player_match_stats",
                             col_types = "c"),
                  error = function(e) {
                    message("    get_player_match_stats read FAILED: ",
                            conditionMessage(e))
                    NULL
                  })
  if (is.null(out) || nrow(out) == 0) {
    message("    get_player_match_stats: no rows")
    return(tibble::tibble(match_id = character(0), player_id = character(0)))
  }
  
  for (col in intersect(PMS_NUMERIC_COLS, names(out))) {
    v <- suppressWarnings(as.numeric(out[[col]]))
    out[[col]] <- ifelse(is.na(v), 0, v)
  }
  
  # Blank cells round-trip from Sheets as "" — normalise to NA so the
  # unmapped-player checks downstream read as missing, not as a player
  # whose id is the empty string.
  if ("player_id" %in% names(out)) {
    pid <- trimws(as.character(out$player_id))
    out$player_id <- ifelse(nzchar(pid), pid, NA_character_)
  }
  
  message("    get_player_match_stats read ", nrow(out), " rows across ",
          length(unique(out$match_id)), " matches, ",
          sum(!is.na(out$player_id)), " resolved to a contest player")
  out
}

# --- Transfers log ----------------------------------------------------------

# All rows from the `transfers` tab: the permanent audit log of roster
# changes (commissioner corrections, window transfers, club-clash
# auto-subs). Fresh read; tolerates a missing or empty tab by returning a
# zero-row tibble with the expected columns, so the Transfers view in the
# squad panel renders its empty state rather than erroring before the tab
# exists. Seeded by seed_transfers_tab.R; window rows will be appended by
# apply_transfers() (TRANSFER_WINDOW.md, Batch 5). Cached via
# get_transfers_cached() in R/cache.R.
TRANSFERS_COLS <- c("transfer_id", "entry_id", "out_player_id",
                    "in_player_id", "type", "effective_gw", "note",
                    "created_at")

get_all_transfers <- function() {
  message(">>> get_all_transfers")
  out <- tryCatch(read_sheet(sheet_id(), sheet = "transfers",
                             col_types = "c"),
                  error = function(e) {
                    message("    get_all_transfers read FAILED ",
                            "(tab may not exist yet): ",
                            conditionMessage(e))
                    NULL
                  })
  if (is.null(out) || nrow(out) == 0) {
    message("    get_all_transfers: no rows")
    out <- tibble::as_tibble(setNames(
      lapply(TRANSFERS_COLS, function(x) character(0)), TRANSFERS_COLS))
    return(out)
  }
  for (col in setdiff(TRANSFERS_COLS, names(out))) {
    out[[col]] <- NA_character_
  }
  message("    get_all_transfers read ", nrow(out), " rows across ",
          length(unique(out$entry_id)), " entries")
  out
}
# ---- DRY_RUN flag helper (file-level; console assignment is overwritten on source) ----
set_dry_run <- function(file, value) {
  stopifnot(is.logical(value), length(value) == 1, !is.na(value))
  x <- readLines(file, warn = FALSE)
  i <- grep("^DRY_RUN\\s*<-", x)
  stopifnot(length(i) == 1)
  x[i] <- paste0("DRY_RUN <- ", if (value) "TRUE" else "FALSE")
  writeLines(x, file)
  message("[set_dry_run] ", file, " -> ", x[i])
  invisible(x[i])
}