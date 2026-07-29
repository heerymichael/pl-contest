# build_2526_scores.R (v4) -----------------------------------------------
# Builds the bundled 2025-26 season scores exhibit data:
#   data/pl2526_scores.rds   — player-level season totals (UNCHANGED
#                              schema/values vs v3; feeds the pool's
#                              pts_2526/pp90_2526 join in
#                              snapshot_pl_reference.R)
#   data/pl2526_stints.rds   — NEW: stint-level rows. A mid-season
#                              mover (e.g. Semenyo BOU -> MCI) gets one
#                              row per club stint, each with that
#                              stint's stats and points. Feeds the
#                              25/26 Scores exhibit (R/scores_2526.R).
#
# v4 changes:
#   - Match dates joined from the fixtures parquet (root-level files
#     only — the player_match_stats subdirectory is a separate
#     dataset) so stints can be ordered chronologically.
#   - Stint construction: per player, matches ordered by date; a stint
#     is a maximal run at one club (rle over club). Handles multiple
#     moves / loan-and-return.
#   - Rare events (own goals, penalty saves) attributed to the stint
#     containing their match via (player_id, match_id) — hard-stop if
#     any event fails to land.
#   - Consistency gates: per-player stint totals must sum to the
#     season total; rare-event counts must reconcile at both levels.
#   - Mover report: every player with >1 stint is printed for review.
#
# v3 changes (retained):
#   - BENCH FIX: only rows with minutes_played > 0 are scored.
#   - Per-category COUNTS and POINTS per player; category points
#     asserted to sum to the engine total (consistency gate).
#   - first_name/surname split (particle-aware); club_code column.
#
# Scoring maths runs through the LIVE R/scoring.R engine; category
# breakdown mirrors it and is verified against it. Own goals (-5) and
# in-play penalty saves (+3) are joined from the rare-events CSV
# (extract_2526_rare_events.R over the raw shotmaps) after the
# consistency gate.
#
# Run from the pl-contest project root (no Sheets auth needed):
#   source("build_2526_scores.R")

library(dplyr)
library(tidyr)
library(arrow)

FOOTBALL_DATA <- path.expand("~/Desktop/football-data/data")
PARQUET_PMS   <- file.path(FOOTBALL_DATA, "parquet", "player_match_stats")
stopifnot(dir.exists(PARQUET_PMS))
source("R/scoring.R")

message(">>> build_2526_scores (v4)")

pms <- open_dataset(PARQUET_PMS) |>
  filter(season == "2025_26") |>
  collect() |>
  mutate(team_id  = as.character(team_id),
         match_id = as.character(match_id))
message("    [scores] player-match rows: ", nrow(pms))

# --- 1. team_id -> club name, three passes ------------------------------

tm <- pms |> distinct(team_id, match_id, home_team, away_team)

mapping <- new.env()
for (tid in unique(tm$team_id)) {
  g <- tm[tm$team_id == tid, ]
  names_common <- Reduce(intersect,
                         lapply(seq_len(nrow(g)), function(i)
                           c(g$home_team[i], g$away_team[i])))
  if (length(names_common) == 1) assign(tid, names_common, envir = mapping)
}
repeat {
  changed <- FALSE
  for (mid in unique(tm$match_id)) {
    g   <- tm[tm$match_id == mid, ]
    ids <- unique(g$team_id)
    if (length(ids) != 2) next
    known <- ids[vapply(ids, exists, logical(1), envir = mapping)]
    if (length(known) == 1) {
      other <- setdiff(ids, known)
      rem   <- setdiff(c(g$home_team[1], g$away_team[1]),
                       get(known, envir = mapping))
      if (length(rem) == 1) { assign(other, rem, envir = mapping); changed <- TRUE }
    }
  }
  if (!changed) break
}
club_lookup <- function(tid)
  if (exists(tid, envir = mapping)) get(tid, envir = mapping) else NA_character_
pms$club <- vapply(pms$team_id, club_lookup, character(1))
for (tid in unique(pms$team_id[is.na(pms$club)])) {
  players <- unique(pms$player_id[pms$team_id == tid])
  votes <- pms |> filter(player_id %in% players, !is.na(club)) |>
    count(club) |> arrange(desc(n))
  g    <- tm[tm$team_id == tid, ]
  cand <- unique(c(g$home_team, g$away_team))
  pick <- votes$club[votes$club %in% cand]
  if (length(pick) > 0) assign(tid, pick[1], envir = mapping)
}
pms$club <- vapply(pms$team_id, club_lookup, character(1))
n_drop <- sum(is.na(pms$club))
if (n_drop > 5) stop("Too many unmapped rows (", n_drop, ")", call. = FALSE)
if (n_drop > 0) {
  message("    [scores] dropping ", n_drop, " unresolvable row(s): ",
          paste(unique(pms$player_name[is.na(pms$club)]), collapse = ", "))
  pms <- pms |> filter(!is.na(club))
}
message("    [scores] clubs mapped: ", length(unique(pms$club)),
        " (expect 20)")

# --- 2. BENCH FIX: only rows where the player actually played -----------

n_before <- nrow(pms)
pms <- pms |>
  mutate(minutes_n = suppressWarnings(as.numeric(minutes_played)),
         minutes_n = ifelse(is.na(minutes_n), 0, minutes_n)) |>
  filter(minutes_n > 0)
message("    [scores] bench/no-minute rows removed: ", n_before - nrow(pms),
        " — scoring ", nrow(pms), " appearance rows")

# --- 2b. Match dates (v4) -----------------------------------------------
# From the fixtures parquet dataset (hive-partitioned sibling of
# player_match_stats).
fixtures <- open_dataset(file.path(FOOTBALL_DATA, "parquet", "fixtures")) |>
  filter(season == "2025_26") |>
  select(match_id, utc_date) |>
  collect() |>
  mutate(match_id   = as.character(match_id),
         match_date = as.Date(sub("T.*", "", as.character(utc_date)))) |>
  distinct(match_id, match_date)
stopifnot("fixtures carry NA dates" = !anyNA(fixtures$match_date))

pms <- pms |> left_join(fixtures, by = "match_id")
if (anyNA(pms$match_date)) {
  stop("No fixture date for ", sum(is.na(pms$match_date)),
       " player-match row(s) — fixtures/pms match_id mismatch",
       call. = FALSE)
}

# Stint key: one row per player-match with resolved club + date.
stint_key <- pms |>
  transmute(player_id = as.character(player_id), match_id,
            club, match_date)
dup_pm <- stint_key |> count(player_id, match_id) |> filter(n > 1)
if (nrow(dup_pm) > 0) stop("Duplicate player-match rows in stint key",
                           call. = FALSE)

# --- 3. Side, result, conceded ------------------------------------------

pms <- pms |>
  mutate(
    is_home            = club == home_team,
    goals_for          = ifelse(is_home, home_score, away_score),
    team_goals_against = ifelse(is_home, away_score, home_score),
    result             = case_when(goals_for > team_goals_against ~ "W",
                                   goals_for < team_goals_against ~ "L",
                                   TRUE                            ~ "D")
  )

# --- 4. Positions --------------------------------------------------------

POS_MAP <- c("G" = "GK", "D" = "DEF", "M" = "MID", "F" = "FWD")
unknown_pos <- setdiff(unique(toupper(trimws(pms$position))), names(POS_MAP))
if (length(unknown_pos) > 0) stop("Unmapped position: ",
                                  paste(unknown_pos, collapse = ", "),
                                  call. = FALSE)
players_dim <- pms |>
  mutate(cpos = unname(POS_MAP[toupper(trimws(position))])) |>
  count(player_id, cpos) |>
  group_by(player_id) |> slice_max(n, n = 1, with_ties = FALSE) |>
  ungroup() |>
  transmute(player_id = as.character(player_id), position = cpos)

# --- 5. Engine scoring (authoritative totals) ----------------------------

stats <- pms |>
  transmute(
    player_id       = as.character(player_id),
    player_name, match_id,
    goals           = shooting_goals,
    assists         = passing_assists,
    total_shots     = shooting_total_shots,
    shots_on_target = shooting_shots_on_target,
    tackles         = defending_tackles,
    interceptions   = defending_interceptions,
    saves           = goalkeeping_saves,
    yellow_cards    = general_yellow_cards,
    red_cards       = general_red_cards,
    minutes_played  = minutes_n,
    team_goals_against, result,
    own_goals = 0, penalty_saves = 0
  )
scored <- score_player_match_stats(as.data.frame(stats),
                                   as.data.frame(players_dim))

# --- 6. Per-category counts + points (mirrors the engine) ---------------

cat_rows <- scored |>
  mutate(
    minutes = .num0(minutes_played),
    g_n   = .num0(goals),            a_n  = .num0(assists),
    sh_n  = .num0(total_shots),      sot_n = .num0(shots_on_target),
    tkl_n = .num0(tackles),          int_n = .num0(interceptions),
    yc_n  = .num0(yellow_cards),     rc_n  = .num0(red_cards),
    sv_n  = .num0(saves),
    conc_n = .num0(team_goals_against),
    yc_charged = ifelse(rc_n > 0, pmin(yc_n, 1), yc_n),
    is_gk  = position == "GK", is_def = position == "DEF",
    cs_flag = conc_n == 0 & minutes > 60,
    cs_n    = as.integer(cs_flag),
    cs_p    = ifelse(cs_flag,
                     c(GK = 8, DEF = 8, MID = 4, FWD = 0)[position], 0),
    w_n = as.integer(toupper(result) == "W"),
    d_n = as.integer(toupper(result) == "D"),
    res_p = ifelse(is_gk,  ifelse(w_n == 1, 5, ifelse(d_n == 1, 2, 0)),
                   ifelse(is_def, ifelse(w_n == 1, 2, ifelse(d_n == 1, 1, 0)), 0)),
    g_p = g_n * 10, a_p = a_n * 6, sh_p = sh_n, sot_p = sot_n,
    tkl_p = tkl_n, int_p = int_n,
    yc_p = yc_charged * -1.5, rc_p = rc_n * -5,
    sv_p  = ifelse(is_gk, sv_n * 2, 0),
    conc_gk_n = ifelse(is_gk, conc_n, 0),
    conc_p    = ifelse(is_gk, conc_n * -2, 0)
  )

season <- cat_rows |>
  group_by(player_id, player_name, position) |>
  summarise(
    apps = n(), minutes = sum(minutes),
    across(c(g_n, a_n, sh_n, sot_n, tkl_n, int_n, yc_n, rc_n,
             cs_n, w_n, d_n, sv_n, conc_gk_n,
             g_p, a_p, sh_p, sot_p, tkl_p, int_p, yc_p, rc_p,
             cs_p, res_p, sv_p, conc_p), sum),
    engine_total = sum(points),
    .groups = "drop"
  ) |>
  mutate(
    total_points = round(g_p + a_p + sh_p + sot_p + tkl_p + int_p +
                           yc_p + rc_p + cs_p + res_p + sv_p + conc_p, 1),
    pp90         = round(ifelse(minutes > 0, total_points / minutes * 90, 0), 1)
  )

# Consistency gate: category breakdown must reproduce the engine
bad <- season |> filter(abs(total_points - round(engine_total, 1)) > 0.05)
if (nrow(bad) > 0) {
  print(head(bad |> select(player_name, total_points, engine_total)))
  stop("Category breakdown disagrees with the engine for ", nrow(bad),
       " players — investigate before shipping", call. = FALSE)
}
message("    [scores] category breakdown == engine total for all ",
        nrow(season), " players \u2713")

# --- 6b. Rare events: own goals + in-play penalty saves ------------------
# From the raw shotmaps via extract_2526_rare_events.R (football-data).
# Joined AFTER the gate: the gate checks parquet-vs-engine; these two
# categories exist only in the shotmaps. Conceded/clean sheets already
# reflect own goals (match scores are team-level), so the -5 is the
# only adjustment; +3 per penalty save attaches to the keeper.

RARE_EVENTS_CSV <- file.path(FOOTBALL_DATA, "pl_2526_rare_events.csv")
if (!file.exists(RARE_EVENTS_CSV)) {
  stop("Missing ", RARE_EVENTS_CSV, " — run ",
       "extract_2526_rare_events.R in football-data first.",
       call. = FALSE)
}
ev <- read.csv(RARE_EVENTS_CSV, stringsAsFactors = FALSE)
ev_agg <- ev |>
  group_by(player_id) |>
  summarise(og_n = sum(event_type == "own_goal"),
            ps_n = sum(event_type == "pen_save"), .groups = "drop")

orphans <- setdiff(ev_agg$player_id, season$player_id)
if (length(orphans) > 0) {
  bad_ev <- ev |> filter(player_id %in% orphans)
  print(bad_ev |> select(event_type, match_id, player_id, player_name))
  stop("Rare-event player(s) not in the season table — investigate ",
       "before shipping", call. = FALSE)
}

season <- season |>
  left_join(ev_agg, by = "player_id") |>
  mutate(
    og_n = ifelse(is.na(og_n), 0L, og_n),
    ps_n = ifelse(is.na(ps_n), 0L, ps_n),
    og_p = og_n * -5,
    ps_p = ps_n * 3,
    total_points = round(total_points + og_p + ps_p, 1),
    pp90         = round(ifelse(minutes > 0,
                                total_points / minutes * 90, 0), 1)
  )

stopifnot(
  "own-goal total drifted from the events CSV" =
    sum(season$og_n) == sum(ev$event_type == "own_goal"),
  "pen-save total drifted from the events CSV" =
    sum(season$ps_n) == sum(ev$event_type == "pen_save")
)
message("    [scores] rare events joined: ", sum(season$og_n),
        " own goals (", sum(season$og_n > 0), " players), ",
        sum(season$ps_n), " penalty saves (",
        sum(season$ps_n > 0), " keepers)")

# --- 7. Club codes + name split ------------------------------------------

CLUB_CODES <- c(
  "Arsenal" = "ARS", "Aston Villa" = "AVL", "Bournemouth" = "BOU",
  "Brentford" = "BRE", "Brighton & Hove Albion" = "BHA",
  "Burnley" = "BUR", "Chelsea" = "CHE", "Crystal Palace" = "CRY",
  "Everton" = "EVE", "Fulham" = "FUL", "Leeds United" = "LEE",
  "Liverpool" = "LIV", "Manchester City" = "MCI",
  "Manchester United" = "MUN", "Newcastle United" = "NEW",
  "Nottingham Forest" = "NFO", "Sunderland" = "SUN",
  "Tottenham Hotspur" = "TOT", "West Ham United" = "WHU",
  "Wolverhampton" = "WOL"
)
club_of <- pms |>
  count(player_id, club) |>
  group_by(player_id) |> slice_max(n, n = 1, with_ties = FALSE) |>
  ungroup() |>
  transmute(player_id = as.character(player_id), club)
missing_code <- setdiff(unique(club_of$club), names(CLUB_CODES))
if (length(missing_code) > 0) stop("No club code for: ",
                                   paste(missing_code, collapse = ", "),
                                   call. = FALSE)

PARTICLES <- c("van", "de", "den", "der", "di", "da", "dos", "del",
               "la", "le", "el", "st.", "st", "mc")
split_name <- function(full) {
  parts <- strsplit(trimws(full), " ")[[1]]
  if (length(parts) <= 1) return(c("", full))
  cut <- length(parts)
  while (cut > 2 && tolower(parts[cut - 1]) %in% PARTICLES) cut <- cut - 1
  if (cut == 2 && tolower(parts[1]) %in% PARTICLES) cut <- 1
  c(paste(parts[seq_len(cut - 1)], collapse = " "),
    paste(parts[cut:length(parts)], collapse = " "))
}
nm <- t(vapply(season$player_name, split_name, character(2)))
season$first_name <- nm[, 1]
season$surname    <- nm[, 2]

season <- season |>
  left_join(club_of, by = "player_id") |>
  mutate(club_code = unname(CLUB_CODES[club])) |>
  select(-engine_total) |>
  arrange(desc(total_points))

message("    [scores] season table: ", nrow(season), " players; top: ",
        season$player_name[1], " (", season$position[1], ", ",
        season$total_points[1], " pts)")

dir.create("data", showWarnings = FALSE)
saveRDS(season, "data/pl2526_scores.rds")

# --- 8. Stint-level table (v4) -------------------------------------------
# One row per (player, club stint). A stint is a maximal chronological
# run at one club — a January mover gets two rows, a loan-out-and-back
# would get three. Single-club players get exactly one stint row whose
# numbers equal their season row.

stint_assign <- stint_key |>
  arrange(player_id, match_date, match_id) |>
  group_by(player_id) |>
  mutate(stint_no = with(rle(club),
                         rep(seq_along(lengths), lengths))) |>
  ungroup()

stint_rows <- cat_rows |>
  mutate(player_id = as.character(player_id)) |>
  inner_join(stint_assign |> select(player_id, match_id, club,
                                    stint_no, match_date),
             by = c("player_id", "match_id"))
stopifnot("stint join changed row count" =
            nrow(stint_rows) == nrow(cat_rows))

stints <- stint_rows |>
  group_by(player_id, player_name, position, club, stint_no) |>
  summarise(
    apps = n(), minutes = sum(minutes),
    stint_from = format(min(match_date), "%b"),
    stint_to   = format(max(match_date), "%b"),
    across(c(g_n, a_n, sh_n, sot_n, tkl_n, int_n, yc_n, rc_n,
             cs_n, w_n, d_n, sv_n, conc_gk_n,
             g_p, a_p, sh_p, sot_p, tkl_p, int_p, yc_p, rc_p,
             cs_p, res_p, sv_p, conc_p), sum),
    .groups = "drop"
  ) |>
  mutate(
    total_points = round(g_p + a_p + sh_p + sot_p + tkl_p + int_p +
                           yc_p + rc_p + cs_p + res_p + sv_p + conc_p, 1),
    pp90         = round(ifelse(minutes > 0, total_points / minutes * 90, 0), 1)
  )

# Rare events attributed to the stint containing their match.
ev_stint <- ev |>
  mutate(player_id = as.character(player_id),
         match_id  = as.character(match_id)) |>
  left_join(stint_assign |> select(player_id, match_id, stint_no),
            by = c("player_id", "match_id"))
if (anyNA(ev_stint$stint_no)) {
  print(ev_stint |> filter(is.na(stint_no)) |>
          select(event_type, match_id, player_id, player_name))
  stop("Rare event(s) failed to land in a stint — (player, match) not ",
       "in the appearance data. Investigate before shipping.",
       call. = FALSE)
}
ev_stint_agg <- ev_stint |>
  group_by(player_id, stint_no) |>
  summarise(og_n2 = sum(event_type == "own_goal"),
            ps_n2 = sum(event_type == "pen_save"), .groups = "drop")

stints <- stints |>
  left_join(ev_stint_agg, by = c("player_id", "stint_no")) |>
  mutate(
    og_n = ifelse(is.na(og_n2), 0L, og_n2),
    ps_n = ifelse(is.na(ps_n2), 0L, ps_n2),
    og_p = og_n * -5,
    ps_p = ps_n * 3,
    total_points = round(total_points + og_p + ps_p, 1),
    pp90         = round(ifelse(minutes > 0,
                                total_points / minutes * 90, 0), 1)
  ) |>
  select(-og_n2, -ps_n2)

# Gate: per-player stint totals must reproduce the season table exactly.
recon <- stints |>
  group_by(player_id) |>
  summarise(stint_sum   = round(sum(total_points), 1),
            stint_apps  = sum(apps),
            stint_mins  = sum(minutes), .groups = "drop") |>
  inner_join(season |> select(player_id, total_points, apps, minutes),
             by = "player_id")
bad2 <- recon |> filter(abs(stint_sum - total_points) > 0.05 |
                          stint_apps != apps | stint_mins != minutes)
if (nrow(bad2) > 0) {
  print(head(bad2))
  stop("Stint rows do not sum to season totals for ", nrow(bad2),
       " player(s) — investigate before shipping", call. = FALSE)
}
stopifnot(
  "stint own-goal total drifted" = sum(stints$og_n) == sum(season$og_n),
  "stint pen-save total drifted" = sum(stints$ps_n) == sum(season$ps_n)
)
message("    [scores] stint totals == season totals for all ",
        nrow(recon), " players \u2713")

# Season-level context columns (exhibit floor is applied per PLAYER so
# a mover's short stint isn't hidden while the long one shows).
stints <- stints |>
  add_count(player_id, name = "n_stints") |>
  left_join(season |> transmute(player_id,
                                season_apps    = apps,
                                season_minutes = minutes),
            by = "player_id") |>
  mutate(club_code = unname(CLUB_CODES[club]))
stopifnot("stint row missing club code" = !anyNA(stints$club_code))

nm2 <- t(vapply(stints$player_name, split_name, character(2)))
stints$first_name <- nm2[, 1]
stints$surname    <- nm2[, 2]

stints <- stints |> arrange(desc(total_points))

movers <- stints |> filter(n_stints > 1) |>
  arrange(player_id, stint_no)
if (nrow(movers) > 0) {
  message("    [scores] MID-SEASON MOVERS (", 
          length(unique(movers$player_id)), " player(s)) — review:")
  for (pid in unique(movers$player_id)) {
    m <- movers |> filter(player_id == pid)
    message("      - ", m$player_name[1], ": ",
            paste(sprintf("%s (%s\u2013%s, %d apps, %.1f pts)",
                          m$club_code, m$stint_from, m$stint_to,
                          m$apps, m$total_points),
                  collapse = " -> "))
  }
} else {
  message("    [scores] no mid-season movers found")
}

saveRDS(stints, "data/pl2526_stints.rds")
message(">>> build_2526_scores complete: ",
        nrow(season), " players -> data/pl2526_scores.rds; ",
        nrow(stints), " stint rows -> data/pl2526_stints.rds. ",
        "Restart the app to pick up the exhibit.")
