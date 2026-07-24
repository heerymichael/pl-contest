# snapshot_pl_reference.R ------------------------------------------------
# Regenerates the local reference-data snapshots that R/cache.R loads
# at startup:
#   data/players_snapshot.rds  (from the `players` tab)
#   data/teams_snapshot.rds    (from the `teams` tab)
#
# The Sheet remains the source of truth. Re-run this after ANY change
# to the players or teams tabs (price refresh, transfer update, club
# data fix), then restart the app.
#
# is_active: R/cache.R filters players on is_active == TRUE. The PL
# players tab doesn't carry the column yet (all 580 are active), so it
# is synthesised as TRUE here. When transfer handling lands, the column
# moves into the tab proper (departing players get is_active = 0, never
# deleted — player_id stability).
#
# Run in two steps per the auth pattern:
#   source("R/sheets.R")
#   source("snapshot_pl_reference.R")

library(dplyr)
library(googlesheets4)

message(">>> snapshot_pl_reference — target Sheet: ", sheet_id())

players <- read_sheet(sheet_id(), sheet = "players",
                      col_types = "iccccdc")
message("    [snapshot] read players tab — ", nrow(players), " rows")

if (!"is_active" %in% names(players)) {
  players$is_active <- TRUE
  message("    [snapshot] is_active not in tab — synthesised TRUE for all")
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