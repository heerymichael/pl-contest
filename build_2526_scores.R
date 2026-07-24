# build_2526_scores.R (v2) -----------------------------------------------
# Builds the bundled 2025-26 season scores exhibit:
#   data/pl2526_scores.rds
#
# v2: no raw-JSON cache needed. Home/away sides derive from a
# three-pass resolution of team_id -> club name, validated against the
# full season data:
#   1. Intersection: a club's name is the one name common to all of a
#      team_id's matches.
#   2. Match propagation: in any match with one resolved id, the other
#      id takes the remaining name.
#   3. Player-identity vote: academy/fringe secondary ids resolve via
#      where their players appear under resolved ids.
# One row in 15,198 (a single-appearance youth player under a one-off
# id) remains unresolvable and is dropped with a logged message.
#
# Scoring runs through the LIVE R/scoring.R ruleset. Known v1
# omissions: penalty saves (+3) and own goals (-5) are not in the
# collected payloads; footnoted in the UI.
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

message(">>> build_2526_scores (v2)")

pms <- open_dataset(PARQUET_PMS) |>
  filter(season == "2025_26") |>
  collect() |>
  mutate(team_id  = as.character(team_id),
         match_id = as.character(match_id))
message("    [scores] player-match rows: ", nrow(pms))

# --- 1. team_id -> club name, three passes ------------------------------

tm <- pms |> distinct(team_id, match_id, home_team, away_team)

# Pass 1: intersection
mapping <- new.env()
for (tid in unique(tm$team_id)) {
  g <- tm[tm$team_id == tid, ]
  names_common <- Reduce(intersect,
                         lapply(seq_len(nrow(g)), function(i)
                           c(g$home_team[i], g$away_team[i])))
  if (length(names_common) == 1) assign(tid, names_common, envir = mapping)
}

# Pass 2: match propagation
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
      if (length(rem) == 1) {
        assign(other, rem, envir = mapping)
        changed <- TRUE
      }
    }
  }
  if (!changed) break
}

# Pass 3: player-identity vote for remaining (academy secondary ids)
club_lookup <- function(tid) {
  if (exists(tid, envir = mapping)) get(tid, envir = mapping) else NA_character_
}
pms$club <- vapply(pms$team_id, club_lookup, character(1))

unresolved <- unique(pms$team_id[is.na(pms$club)])
for (tid in unresolved) {
  players <- unique(pms$player_id[pms$team_id == tid])
  votes <- pms |>
    filter(player_id %in% players, !is.na(club)) |>
    count(club) |> arrange(desc(n))
  g    <- tm[tm$team_id == tid, ]
  cand <- unique(c(g$home_team, g$away_team))
  pick <- votes$club[votes$club %in% cand]
  if (length(pick) > 0) assign(tid, pick[1], envir = mapping)
}
pms$club <- vapply(pms$team_id, club_lookup, character(1))

n_drop <- sum(is.na(pms$club))
if (n_drop > 5) {
  stop("Too many unmapped rows (", n_drop, ") — resolution failed; ",
       "investigate before building", call. = FALSE)
}
if (n_drop > 0) {
  message("    [scores] dropping ", n_drop, " unresolvable row(s): ",
          paste(unique(pms$player_name[is.na(pms$club)]), collapse = ", "))
  pms <- pms |> filter(!is.na(club))
}
message("    [scores] clubs mapped: ", length(unique(pms$club)),
        " (expect 20)")

# --- 2. Side, result, conceded ------------------------------------------

pms <- pms |>
  mutate(
    is_home            = club == home_team,
    goals_for          = ifelse(is_home, home_score, away_score),
    team_goals_against = ifelse(is_home, away_score, home_score),
    result             = case_when(goals_for > team_goals_against ~ "W",
                                   goals_for < team_goals_against ~ "L",
                                   TRUE                            ~ "D")
  )

# --- 3. Positions (API G/D/M/F -> contest; modal per player) ------------

POS_MAP <- c("G" = "GK", "D" = "DEF", "M" = "MID", "F" = "FWD")
unknown_pos <- setdiff(unique(toupper(trimws(pms$position))), names(POS_MAP))
if (length(unknown_pos) > 0) {
  stop("Unmapped API position value(s): ",
       paste(unknown_pos, collapse = ", "), call. = FALSE)
}
players_dim <- pms |>
  mutate(cpos = unname(POS_MAP[toupper(trimws(position))])) |>
  count(player_id, cpos) |>
  group_by(player_id) |>
  slice_max(n, n = 1, with_ties = FALSE) |>
  ungroup() |>
  transmute(player_id = as.character(player_id), position = cpos)

# --- 4. Score through the live engine -----------------------------------

stats <- pms |>
  transmute(
    player_id       = as.character(player_id),
    player_name,
    match_id,
    goals           = shooting_goals,
    assists         = passing_assists,
    total_shots     = shooting_total_shots,
    shots_on_target = shooting_shots_on_target,
    tackles         = defending_tackles,
    interceptions   = defending_interceptions,
    saves           = goalkeeping_saves,
    yellow_cards    = general_yellow_cards,
    red_cards       = general_red_cards,
    minutes_played,
    team_goals_against,
    result,
    own_goals       = 0,
    penalty_saves   = 0
  )

scored <- score_player_match_stats(as.data.frame(stats),
                                   as.data.frame(players_dim))

# --- 5. Season aggregate -------------------------------------------------

club_of <- pms |>
  count(player_id, club) |>
  group_by(player_id) |>
  slice_max(n, n = 1, with_ties = FALSE) |>
  ungroup() |>
  transmute(player_id = as.character(player_id), club)

season <- scored |>
  mutate(minutes = .num0(minutes_played),
         cs      = .num0(team_goals_against) == 0 & minutes > 60) |>
  group_by(player_id, player_name, position) |>
  summarise(
    apps          = sum(minutes > 0),
    minutes       = sum(minutes),
    goals         = sum(.num0(goals)),
    assists       = sum(.num0(assists)),
    tackles       = sum(.num0(tackles)),
    interceptions = sum(.num0(interceptions)),
    clean_sheets  = sum(cs & position %in% c("GK", "DEF", "MID")),
    saves         = sum(.num0(saves)),
    total_points  = round(sum(points), 1),
    .groups = "drop"
  ) |>
  mutate(ppg = round(ifelse(apps > 0, total_points / apps, 0), 1)) |>
  left_join(club_of, by = "player_id") |>
  arrange(desc(total_points))

message("    [scores] season table: ", nrow(season), " players; top: ",
        season$player_name[1], " (", season$position[1], ", ",
        season$total_points[1], " pts)")

dir.create("data", showWarnings = FALSE)
saveRDS(season, "data/pl2526_scores.rds")
message(">>> build_2526_scores complete — data/pl2526_scores.rds written")
