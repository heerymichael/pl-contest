# snapshot_pl_reference.R (v3) --------------------------------------------
# Regenerates the local reference snapshots that R/cache.R loads:
#   data/players_snapshot.rds  /  data/teams_snapshot.rds
#
# v3: joins informational 25/26 columns (pts_2526, pp90_2526) into the
# players snapshot from data/pl2526_scores.rds, via normalised
# full-name matching (no shared id exists between FanTeam and the
# API). Non-matches — promoted-club players, summer arrivals — stay NA
# and render as an em dash in the pool. Re-run after any players/teams
# tab change OR after rebuilding the 25/26 scores, then restart.
#
#   source("R/sheets.R")
#   source("snapshot_pl_reference.R")

library(dplyr)
library(googlesheets4)

message(">>> snapshot_pl_reference (v3) — target Sheet: ", sheet_id())

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

# --- 25/26 informational join -------------------------------------------
norm_name <- function(x) {
  x <- iconv(x, to = "ASCII//TRANSLIT")
  gsub("[^a-z]", "", tolower(x))
}

if (file.exists("data/pl2526_scores.rds")) {
  sc <- readRDS("data/pl2526_scores.rds") |>
    mutate(nn = norm_name(player_name)) |>
    group_by(nn) |>
    slice_max(minutes, n = 1, with_ties = FALSE) |>
    ungroup() |>
    transmute(nn, pts_2526 = total_points, pp90_2526 = pp90)
  players <- players |>
    select(-any_of(c("pts_2526", "pp90_2526"))) |>
    mutate(nn = norm_name(name)) |>
    left_join(sc, by = "nn") |>
    select(-nn)
  message("    [snapshot] 25/26 join: ", sum(!is.na(players$pts_2526)),
          " of ", nrow(players), " pool players matched")
} else {
  players$pts_2526 <- NA_real_
  players$pp90_2526 <- NA_real_
  message("    [snapshot] data/pl2526_scores.rds missing — 25/26 columns NA")
}

teams <- read_sheet(sheet_id(), sheet = "teams", col_types = "iccc")
message("    [snapshot] read teams tab — ", nrow(teams), " rows")
stopifnot(nrow(players) > 0, nrow(teams) == 20)

dir.create("data", showWarnings = FALSE)
saveRDS(players, "data/players_snapshot.rds")
saveRDS(teams,   "data/teams_snapshot.rds")
message(">>> snapshot_pl_reference complete — ",
        nrow(players), " players, ", nrow(teams),
        " teams. Restart the app to pick them up.")