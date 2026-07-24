# snapshot_pl_reference.R ------------------------------------------------
# Regenerates the local reference-data snapshots that R/cache.R loads
# at startup:
#   data/players_snapshot.rds  (from the `players` tab)
#   data/teams_snapshot.rds    (from the `teams` tab)
#
# Re-run after ANY change to the players or teams tabs, then restart
# the app. Column-agnostic: reads all columns as character then
# restores the known numeric types, so schema additions (first_name,
# surname, is_active...) never require edits here.
#
#   source("R/sheets.R")
#   source("snapshot_pl_reference.R")

library(dplyr)
library(googlesheets4)

message(">>> snapshot_pl_reference — target Sheet: ", sheet_id())

players <- read_sheet(sheet_id(), sheet = "players", col_types = "c") |>
  mutate(player_id     = as.integer(player_id),
         fanteam_price = as.numeric(fanteam_price))
message("    [snapshot] read players tab — ", nrow(players), " rows, ",
        ncol(players), " cols")

if (!"is_active" %in% names(players)) {
  players$is_active <- TRUE
  message("    [snapshot] is_active not in tab — synthesised TRUE for all")
} else {
  players$is_active <- as.logical(as.integer(players$is_active))
}

teams <- read_sheet(sheet_id(), sheet = "teams", col_types = "iccc")
message("    [snapshot] read teams tab — ", nrow(teams), " rows")

stopifnot(nrow(players) > 0, nrow(teams) == 20)

dir.create("data", showWarnings = FALSE)
saveRDS(players, "data/players_snapshot.rds")
saveRDS(teams,   "data/teams_snapshot.rds")

message(">>> snapshot_pl_reference complete — ",
        nrow(players), " players (", sum(players$is_active), " active), ",
        nrow(teams), " teams. Restart the app to pick them up.")
