# check_club_limit.R ---------------------------------------------------------
#
# Read-only diagnostic. Writes NOTHING, anywhere.
#
# Overlays the club assignments in data/fanteam_salaries.csv (the refreshed
# FanTeam export) onto every entry's roster and reports any entry that would
# hold two or more players from the same club once those assignments are
# applied to the players tab. Also lists every OWNED player whose club has
# changed, so the September window validation and any players-tab refresh
# can be planned against real ownership.
#
# Run after:  source("R/sheets.R")   (auth + sheet_id())
#
# PERIOD-AWARE. roster_picks holds one full roster per period (period 1 =
# GW1-2, period 2 = GW3-19). Clashes are measured on the roster that scores
# CHECK_PERIOD, selected exactly as R/bestball.R's .bb_entry_roster() does:
# rows tagged CHECK_PERIOD if the entry has any, otherwise all its rows.
# Counting across periods would report every transfer as a clash between
# the outgoing and incoming player's clubs.
#
# Notes:
#   * FanTeam has renamed Coventry's code COV -> CVC in the new export.
#     This script normalises CVC back to COV before comparing, so a pure
#     code rename is NOT reported as a club change.
#   * All IDs are handled as character throughout. player_id and
#     fanteam_id are never coerced to numeric.
#
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(googlesheets4)
  library(dplyr)
  library(readr)
})

message(">>> SOURCED check_club_limit.R at ", format(Sys.time()))

SALARIES_CSV <- "data/fanteam_salaries.csv"

# The period whose roster is validated. 2L from GW3 onward.
CHECK_PERIOD <- 2L
ROSTER_SIZE  <- 15L

# FanTeam code renames that are NOT real transfers.
CODE_RENAMES <- c(CVC = "COV")

if (!exists("sheet_id")) {
  stop("source(\"R/sheets.R\") first: sheet_id() and auth are required.")
}

# --- Read: Sheet (live) ----------------------------------------------------

players <- read_sheet(sheet_id(), sheet = "players",      col_types = "c")
picks   <- read_sheet(sheet_id(), sheet = "roster_picks", col_types = "c")
entries <- read_sheet(sheet_id(), sheet = "entries",      col_types = "c")

message("    [club] players ", nrow(players),
        " | roster_picks ", nrow(picks),
        " | entries ", nrow(entries))

players <- players |>
  mutate(
    player_id  = as.character(player_id),
    fanteam_id = as.character(fanteam_id),
    team       = toupper(trimws(as.character(team)))
  )

picks <- picks |>
  mutate(
    entry_id  = as.character(entry_id),
    player_id = as.character(player_id),
    period    = if ("period" %in% names(picks))
      suppressWarnings(as.integer(period)) else NA_integer_
  )

# --- Period structure ------------------------------------------------------

period_sizes <- picks |>
  filter(!is.na(period)) |>
  count(entry_id, period, name = "n")
bad_period_sizes <- period_sizes |> filter(n != ROSTER_SIZE)

has_period <- picks |>
  filter(period == CHECK_PERIOD) |>
  distinct(entry_id) |>
  pull(entry_id)

# The roster that scores CHECK_PERIOD, mirroring .bb_entry_roster().
picks_all <- picks
picks <- bind_rows(
  picks_all |> filter(entry_id %in% has_period, period == CHECK_PERIOD),
  picks_all |> filter(!entry_id %in% has_period)
)

message("    [club] period ", CHECK_PERIOD, " rosters: ", length(has_period),
        " entries | fallback (all rows): ",
        length(unique(picks_all$entry_id)) - length(has_period),
        " entries | blank period cells: ", sum(is.na(picks_all$period)))

if (nrow(bad_period_sizes) > 0) {
  message("\n!!! (entry, period) groups not holding ", ROSTER_SIZE, " rows:")
  print(bad_period_sizes, n = Inf)
}

entry_names <- entries |>
  transmute(
    entry_id   = as.character(entry_id),
    entry_name = coalesce(
      if ("entry_name" %in% names(entries)) entry_name else NA_character_,
      if ("team_name"  %in% names(entries)) team_name  else NA_character_,
      entry_id
    )
  )

# --- Read: refreshed FanTeam export (local) --------------------------------

salaries <- read_csv(SALARIES_CSV, col_types = cols(.default = col_character()))

salaries <- salaries |>
  mutate(
    PlayerID = as.character(PlayerID),
    new_club = toupper(trimws(Club)),
    new_club = if_else(new_club %in% names(CODE_RENAMES),
                       unname(CODE_RENAMES[new_club]), new_club)
  ) |>
  select(fanteam_id = PlayerID, new_club)

message("    [club] salaries csv ", nrow(salaries), " rows")

# --- Overlay ---------------------------------------------------------------

players_new <- players |>
  left_join(salaries, by = "fanteam_id") |>
  mutate(
    club_effective = coalesce(new_club, team),   # unmatched keep current club
    club_changed   = !is.na(new_club) & new_club != team
  )

owned_ids <- unique(picks$player_id)

moved_owned <- players_new |>
  filter(club_changed, player_id %in% owned_ids) |>
  select(player_id, name, old_club = team, new_club)

message("\n=== OWNED (period ", CHECK_PERIOD,
        " rosters) players whose club has changed (", nrow(moved_owned),
        ") ===")
if (nrow(moved_owned) == 0) {
  message("    none")
} else {
  for (i in seq_len(nrow(moved_owned))) {
    message("    ", moved_owned$name[i], "  ",
            moved_owned$old_club[i], " -> ", moved_owned$new_club[i],
            "  (player_id ", moved_owned$player_id[i], ")")
  }
}

# --- Prospective club-limit breaches per entry -----------------------------

roster <- picks |>
  left_join(players_new |>
              select(player_id, name, club_effective, club_changed, team),
            by = "player_id") |>
  left_join(entry_names, by = "entry_id")

unknown <- roster |> filter(is.na(club_effective))
if (nrow(unknown) > 0) {
  message("\n!!! ", nrow(unknown),
          " roster pick(s) reference player_ids missing from the players tab:")
  print(unknown |> count(entry_id, player_id))
}

clashes <- roster |>
  filter(!is.na(club_effective)) |>
  count(entry_id, entry_name, club_effective, name = "n") |>
  filter(n > 1)

message("\n=== PROSPECTIVE club-limit breaches under new assignments, period ",
        CHECK_PERIOD, " rosters (", nrow(clashes), ") ===")

if (nrow(clashes) == 0) {
  message("    none - every roster still satisfies one player per club")
} else {
  for (i in seq_len(nrow(clashes))) {
    members <- roster |>
      filter(entry_id == clashes$entry_id[i],
             club_effective == clashes$club_effective[i]) |>
      mutate(tag = if_else(club_changed,
                           paste0(name, " [moved from ", team, "]"),
                           name))
    message("    ", clashes$entry_name[i],
            "  (entry ", clashes$entry_id[i], ")  ",
            clashes$club_effective[i], " x", clashes$n[i], ": ",
            paste(members$tag, collapse = ", "))
  }
}

# --- Self-verification -----------------------------------------------------

stopifnot(
  nrow(picks) > 0,
  nrow(players_new) == nrow(players),
  !any(duplicated(salaries$fanteam_id)),
  length(unique(picks$entry_id)) == length(unique(picks_all$entry_id)),
  all(picks$period[picks$entry_id %in% has_period] == CHECK_PERIOD,
      na.rm = TRUE)
)

message("\n>>> check_club_limit.R complete. Nothing was written.")