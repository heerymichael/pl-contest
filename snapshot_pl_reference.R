# snapshot_pl_reference.R (v4) --------------------------------------------
# Regenerates the local reference snapshots that R/cache.R loads:
#   data/players_snapshot.rds  /  data/teams_snapshot.rds
#
# v4: the 25/26 informational join (pts_2526, pp90_2526) is now keyed on
# api_player_id — the durable TheStatsAPI bridge seeded by
# map_players_2526.R — with normalised full-name matching retained only
# as a fallback for rows whose api_player_id is blank. Non-matches —
# promoted-club players, summer arrivals — stay NA and render as an em
# dash in the pool. Re-run after any players/teams tab change OR after
# rebuilding the 25/26 scores, then restart.
#
#   source("R/sheets.R")
#   source("snapshot_pl_reference.R")

library(dplyr)
library(googlesheets4)

message(">>> snapshot_pl_reference (v4) — target Sheet: ", sheet_id())

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
# chartr first: iconv ASCII//TRANSLIT drops stroked letters (Đ, Ł, Ø)
# on some platforms instead of converting them.
norm_name <- function(x) {
  x <- chartr("\u0110\u0111\u0141\u0142\u00D8\u00F8", "DdLlOo", x)
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  x[is.na(x)] <- ""
  gsub("[^a-z]", "", tolower(x))
}

if (file.exists("data/pl2526_scores.rds")) {
  sc <- readRDS("data/pl2526_scores.rds")

  # Primary: id join (api_player_id <-> scores player_id)
  sc_id <- sc |>
    transmute(api_player_id = player_id,
              pts_id = total_points, ppg_id = pp90)

  # Fallback: normalised-name join for blank api_player_id rows
  sc_nm <- sc |>
    mutate(nn = norm_name(player_name)) |>
    group_by(nn) |>
    slice_max(minutes, n = 1, with_ties = FALSE) |>
    ungroup() |>
    transmute(nn, pts_nm = total_points, ppg_nm = pp90)

  players <- players |>
    select(-any_of(c("pts_2526", "pp90_2526"))) |>
    mutate(nn = norm_name(name)) |>
    left_join(sc_id, by = "api_player_id") |>
    left_join(sc_nm, by = "nn") |>
    mutate(pts_2526  = coalesce(pts_id, pts_nm),
           pp90_2526 = coalesce(ppg_id, ppg_nm)) |>
    select(-nn, -pts_id, -ppg_id, -pts_nm, -ppg_nm)

  n_id <- sum(!is.na(players$api_player_id) & players$api_player_id != "" &
              !is.na(players$pts_2526))
  n_all <- sum(!is.na(players$pts_2526))
  message("    [snapshot] 25/26 join: ", n_all, " of ", nrow(players),
          " pool players matched (", n_id, " by id, ",
          n_all - n_id, " by name fallback)")
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
