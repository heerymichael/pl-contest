# refresh_fanteam_players.R -----------------------------------------------
# Daily additive refresh of the `players` tab from a fresh FanTeam 26/27 PL
# export. Companion to ingest_fanteam_players.R (the one-off original
# build) — this is the repeatable version for the run-up to lock, matching
# on fanteam_id rather than rebuilding the tab.
#
# INPUT: a FanTeam export CSV (columns Tournament, PlayerID, Name, FName,
# Club, Lineup, Position, Price), saved to data/fanteam_salaries.csv —
# overwrite that same file with each day's fresh download, so CSV_PATH
# below never needs editing.
#
# What it does, matched on fanteam_id (= FanTeam's PlayerID):
#   - Existing player, still in the export -> update team, position,
#     fanteam_price, name in place. player_id NEVER changes.
#   - Existing player, missing from the export -> left in place,
#     is_active set to FALSE. Not deleted (roster_picks references
#     player_id and must stay valid).
#   - Player reappears in a later export having previously gone
#     inactive -> is_active flips back to TRUE automatically.
#   - New player, not seen before -> appended with a fresh player_id
#     continuing from the current max. api_player_id starts blank, same
#     as day one (re-run map_players_2526.R after if you want the stats
#     bridge for new arrivals).
#
# Lineup status (expected/unexpected/possible/injured/suspended) is
# deliberately NOT captured — not needed for a multi-week contest.
#
# Safety: DRY_RUN is TRUE by default. A dry run reads the sheet, reads the
# CSV, computes and prints the full change summary, and writes the
# proposed full players tab to /tmp/refresh_fanteam_players_review.csv —
# nothing live is touched. Review it, then EDIT THE FLAG BELOW IN THIS
# FILE and re-run. (Console assignment of DRY_RUN is overwritten on
# source() — the OWN_GOAL_ALLOWANCE lesson, same as map_players_2526.R.)
#
# Usage (each morning):
#   1. Overwrite data/fanteam_salaries.csv with today's FanTeam export.
#   2. source("R/sheets.R")                  # let auth complete first
#   3. source("refresh_fanteam_players.R")   # dry run — review the output
#   4. Edit DRY_RUN below to FALSE and re-run to commit.
# ---------------------------------------------------------------------------

DRY_RUN <- TRUE

CSV_PATH <- "data/fanteam_salaries.csv"   # overwritten with each day's export

library(dplyr)
library(readr)
library(tibble)
library(googlesheets4)

message(">>> refresh_fanteam_players — target Sheet: ", sheet_id(),
        if (DRY_RUN) "  [DRY RUN]" else "  [LIVE WRITE]")

if (!file.exists(CSV_PATH)) {
  stop(">>> [refresh] input not found: ", CSV_PATH,
       " — point CSV_PATH at today's FanTeam export", call. = FALSE)
}

# --- Read + map the new export -------------------------------------------

raw <- read_csv(CSV_PATH,
                col_types = cols(.default = col_character(),
                                 Price = col_double()))
message("    [refresh] read ", nrow(raw), " rows from ", CSV_PATH)

POSITION_MAP <- c(goalkeeper = "GK", defender = "DEF",
                  midfielder = "MID", forward = "FWD")
CLUB_MAP     <- c(CVC = "COV")   # FanTeam quirks -> contest short_code

pos_in <- tolower(trimws(raw$Position))
bad_pos <- setdiff(unique(pos_in), names(POSITION_MAP))
if (length(bad_pos) > 0) {
  stop(">>> [refresh] unmapped position value(s): ",
       paste(bad_pos, collapse = ", "), call. = FALSE)
}

club_in <- toupper(trimws(raw$Club))
club_mapped <- ifelse(club_in %in% names(CLUB_MAP),
                      CLUB_MAP[club_in], club_in)

teams <- read_sheet(sheet_id(), sheet = "teams", col_types = "iccc")
bad_clubs <- setdiff(unique(club_mapped), teams$short_code)
if (length(bad_clubs) > 0) {
  stop(">>> [refresh] club code(s) not in teams tab: ",
       paste(bad_clubs, collapse = ", "),
       " — extend CLUB_MAP or fix the teams tab", call. = FALSE)
}

# Display name: "FName Name", collapsing the doubled-name case
# (FanTeam lists e.g. Gabriel/Gabriel), and trimming stray double spaces.
fn <- trimws(raw$FName)
sn <- trimws(raw$Name)
display_name <- ifelse(
  tolower(fn) == tolower(sn) & nzchar(fn),
  sn,
  trimws(gsub("\\s+", " ", paste(fn, sn)))
)

new_raw <- tibble(
  fanteam_id    = as.character(raw$PlayerID),
  name          = display_name,
  team          = club_mapped,
  position      = unname(POSITION_MAP[pos_in]),
  fanteam_price = raw$Price
)

stopifnot(!anyDuplicated(new_raw$fanteam_id))

# --- Read the current players tab -----------------------------------------

players <- read_sheet(sheet_id(), sheet = "players", col_types = "c") |>
  mutate(player_id     = as.integer(player_id),
         fanteam_price = as.numeric(fanteam_price))
message("    [refresh] read players tab — ", nrow(players), " rows")

if (!"is_active" %in% names(players)) {
  players$is_active <- TRUE
  message("    [refresh] is_active not in tab — synthesised TRUE for all")
} else {
  players$is_active <- as.logical(as.integer(players$is_active))
}

# --- Diff --------------------------------------------------------------

matched_ids   <- intersect(players$fanteam_id, new_raw$fanteam_id)
newly_gone    <- setdiff(players$fanteam_id, new_raw$fanteam_id)
brand_new     <- setdiff(new_raw$fanteam_id, players$fanteam_id)
reactivated   <- players |>
  filter(fanteam_id %in% matched_ids, is_active == FALSE) |>
  pull(fanteam_id)

old_by_id <- players |> filter(fanteam_id %in% matched_ids) |>
  select(fanteam_id, old_team = team, old_position = position,
         old_price = fanteam_price, old_name = name)
new_by_id <- new_raw |> filter(fanteam_id %in% matched_ids)

changes <- old_by_id |>
  left_join(new_by_id, by = "fanteam_id") |>
  mutate(
    club_changed  = old_team != team,
    price_changed = old_price != fanteam_price,
    name_changed  = old_name != name
  )

message("    [refresh] matched: ", length(matched_ids),
        " | newly inactive: ", length(newly_gone),
        " | reactivated: ", length(reactivated),
        " | brand new: ", length(brand_new))
message("    [refresh] of matched — club changes: ", sum(changes$club_changed),
        " | price changes: ", sum(changes$price_changed),
        " | name changes: ", sum(changes$name_changed))

if (sum(changes$club_changed) > 0) {
  message("    [refresh] club changes:")
  cc <- changes |> filter(club_changed)
  for (i in seq_len(nrow(cc))) {
    message("      - ", cc$name[i], ": ", cc$old_team[i], " -> ", cc$team[i])
  }
}
if (length(newly_gone) > 0) {
  message("    [refresh] newly inactive:")
  gone_rows <- players |> filter(fanteam_id %in% newly_gone)
  for (i in seq_len(nrow(gone_rows))) {
    message("      - ", gone_rows$name[i], " (", gone_rows$team[i], ")")
  }
}
if (length(brand_new) > 0) {
  message("    [refresh] brand new:")
  new_rows <- new_raw |> filter(fanteam_id %in% brand_new)
  for (i in seq_len(nrow(new_rows))) {
    message("      - ", new_rows$name[i], " (", new_rows$team[i], ", ",
            new_rows$position[i], ", £", new_rows$fanteam_price[i], ")")
  }
}

# --- Build the updated tab -------------------------------------------------

# Matched rows: overwrite team/position/price/name from the new export,
# force is_active TRUE (covers straight matches and reactivations alike).
players_matched <- players |>
  filter(fanteam_id %in% matched_ids) |>
  select(-team, -position, -fanteam_price, -name) |>
  left_join(new_raw, by = "fanteam_id") |>
  mutate(is_active = TRUE)

# Gone rows: anything not in today's export — whether it just dropped
# out or was already inactive from a prior day — keep as-is, is_active
# FALSE. (newly_gone is the complement of matched_ids within players, so
# together players_matched + players_gone already cover every existing
# row; no separate "still gone" branch is needed.)
players_gone <- players |>
  filter(fanteam_id %in% newly_gone) |>
  mutate(is_active = FALSE)

# Brand new rows: fresh player_id continuing from current max, blank
# api_player_id, active.
next_id <- max(players$player_id, na.rm = TRUE) + 1L
players_new <- new_raw |>
  filter(fanteam_id %in% brand_new) |>
  mutate(
    player_id     = next_id + row_number() - 1L,
    api_player_id = "",
    is_active     = TRUE
  ) |>
  select(player_id, name, team, position, fanteam_id, fanteam_price,
         api_player_id, is_active)

updated_players <- bind_rows(
  players_matched, players_gone, players_new
) |>
  select(player_id, name, team, position, fanteam_id, fanteam_price,
         api_player_id, is_active) |>
  arrange(player_id)

stopifnot(!anyDuplicated(updated_players$player_id),
          !anyDuplicated(updated_players$fanteam_id))

message("    [refresh] final tab: ", nrow(updated_players), " rows, ",
        sum(updated_players$is_active), " active, ",
        sum(!updated_players$is_active), " inactive")

# Same within-club duplicate-name eyeball check as the original ingest.
dupes <- updated_players |> filter(is_active) |>
  count(team, name) |> filter(n > 1)
if (nrow(dupes) > 0) {
  message("!!! [refresh] same name twice within a club (active) — REVIEW:")
  for (i in seq_len(nrow(dupes))) {
    message("    - ", dupes$team[i], " ", dupes$name[i],
            " (x", dupes$n[i], ")")
  }
} else {
  message("    [refresh] no within-club duplicate active names")
}

# --- Write / review --------------------------------------------------------

if (DRY_RUN) {
  review_path <- "/tmp/refresh_fanteam_players_review.csv"
  write_csv(updated_players, review_path)
  message(">>> [refresh] DRY RUN complete — nothing written to the Sheet. ",
          "Full proposed tab at ", review_path,
          ". Edit DRY_RUN to FALSE and re-run to commit.")
} else {
  googlesheets4::sheet_write(updated_players, ss = sheet_id(), sheet = "players")
  message(">>> [refresh] LIVE WRITE complete — players tab now ",
          nrow(updated_players), " rows (",
          sum(updated_players$is_active), " active).")
  
  # Hard-verify the write by re-reading, per map_players_2526.R convention.
  check <- read_sheet(sheet_id(), sheet = "players", col_types = "c")
  if (nrow(check) != nrow(updated_players)) {
    warning(">>> [refresh] VERIFY MISMATCH — sheet has ", nrow(check),
            " rows, expected ", nrow(updated_players))
  } else {
    message("    [refresh] verified — re-read confirms ", nrow(check), " rows")
  }
}
