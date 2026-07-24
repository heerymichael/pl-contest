# R/scoring.R ------------------------------------------------------------------
# Pure scoring engine for the PL contest.
#
# No side effects, no Sheets reads, no caching — callers supply the data
# frames and get points back. Because nothing is precomputed or stored, a
# scoring correction here fixes all history the moment the leaderboard
# re-renders.
#
# Calibrated PL ruleset (from the 25/26 full-season analysis, locked
# 22 Jul 2026):
#   All positions:
#     goal +10, assist +6, shot +1, shot on target +1
#     (the API's shot counts include goals, so each goal nets 12)
#     tackle +1, interception +1
#     yellow −1.5, red −5; a red charges at most ONE yellow, so any
#     yellow+red combination lands on −6.5
#     own goal −5 (auto-detected from the shotmap at ingest)
#   Clean sheets (team concedes 0 across the entire match AND the player
#   played MORE than 60 minutes): GK +8, DEF +8, MID +4, FWD 0
#   Result points:
#     GK:  win +5, draw +2
#     DEF: win +2, draw +1
#   GK only:
#     save +2, penalty save (in play) +3,
#     goal conceded −2 (team-level: every GK row in the match is charged
#     the full match's conceded — accepted simplification, manual
#     adjustment if a mid-match GK substitution ever makes it matter)
#
#   NO stage multipliers — flat league scoring, every gameweek worth the
#   same. NO shootout columns — league matches cannot go to penalties.
#
# Main entry points:
#   score_player_match_stats(stats, players)  -> stats + points columns
#   score_entries(scored, picks, entries)     -> per-entry raw totals
#                                                (superseded by bestball
#                                                for the leaderboard —
#                                                kept for diagnostics)
#   entry_player_points(scored, picks)        -> per-entry per-player
#                                                breakdown
# The bestball leaderboard lives in R/bestball.R and consumes
# score_player_match_stats() output.
# ------------------------------------------------------------------------------

# Coerce a possibly-NA / possibly-character column from the Sheet to numeric 0+
.num0 <- function(x) {
  out <- suppressWarnings(as.numeric(x))
  ifelse(is.na(out), 0, out)
}

# --- Per player-match scoring ---------------------------------------------------

# stats:   rows from the player_match_stats tab
# players: data frame with player_id and position (the contest's positions,
#          which are authoritative — never the API's)
#
# Returns stats joined with position plus: base_points, points.
# (points == base_points — no multipliers in league scoring. Both columns
# are kept so downstream code shared with the WC app keeps working.)
score_player_match_stats <- function(stats, players) {
  message(">>> score_player_match_stats: ", nrow(stats), " stat rows, ",
          nrow(players), " players")

  pos <- players
  pos$player_id <- as.character(pos$player_id)
  stats$player_id <- as.character(stats$player_id)

  df <- merge(stats, pos[, c("player_id", "position")],
              by = "player_id", all.x = TRUE)

  missing_pos <- unique(df$player_name[is.na(df$position)])
  if (length(missing_pos) > 0) {
    stop("score_player_match_stats: no contest position for: ",
         paste(missing_pos, collapse = ", "),
         " — player_id mismatch between player_match_stats and players tab")
  }

  goals           <- .num0(df$goals)
  assists         <- .num0(df$assists)
  total_shots     <- .num0(df$total_shots)
  shots_on_target <- .num0(df$shots_on_target)
  tackles         <- .num0(df$tackles)
  interceptions   <- .num0(df$interceptions)
  saves           <- .num0(df$saves)
  yellows         <- .num0(df$yellow_cards)
  reds            <- .num0(df$red_cards)
  minutes         <- .num0(df$minutes_played)
  conceded        <- .num0(df$team_goals_against)
  own_goals       <- .num0(df$own_goals)
  pen_saves       <- .num0(df$penalty_saves)

  is_gk  <- df$position == "GK"
  is_def <- df$position == "DEF"
  result  <- toupper(trimws(as.character(df$result)))
  is_win  <- result == "W"
  is_draw <- result == "D"

  # Standard scoring — all positions
  attacking <- goals * 10 + assists * 6 + total_shots * 1 + shots_on_target * 1
  defending <- tackles * 1 + interceptions * 1

  # Cards: a red charges at most one yellow (second yellow is scored AS the
  # red, added to the first yellow: -1.5 - 5 = -6.5)
  yellows_charged <- ifelse(reds > 0, pmin(yellows, 1), yellows)
  cards <- yellows_charged * -1.5 + reds * -5

  # Clean sheets: team-level zero across the whole match, strictly >60 minutes
  cs_eligible <- conceded == 0 & minutes > 60
  cs_value <- c(GK = 8, DEF = 8, MID = 4, FWD = 0)[df$position]
  clean_sheet <- ifelse(cs_eligible, cs_value, 0)

  # Result points: GK win +5 / draw +2; DEF win +2 / draw +1
  result_pts <- ifelse(is_gk,  ifelse(is_win, 5, ifelse(is_draw, 2, 0)),
               ifelse(is_def, ifelse(is_win, 2, ifelse(is_draw, 1, 0)), 0))

  # GK-specific
  gk_points <- ifelse(is_gk,
                      saves * 2 + pen_saves * 3 + conceded * -2,
                      0)

  df$base_points <- attacking + defending + cards + clean_sheet +
    result_pts + gk_points + own_goals * -5
  df$points <- df$base_points

  message(">>> score_player_match_stats: scored ", nrow(df), " rows, ",
          "total points in play ", round(sum(df$points), 1))
  df
}

# --- Per-entry aggregation -------------------------------------------------------

# Raw sum of all rostered players' points — NOT the contest leaderboard
# (that's bestball_leaderboard() in R/bestball.R). Kept for diagnostics
# and sanity checks.
score_entries <- function(scored, picks, entries) {
  message(">>> score_entries: ", nrow(entries), " entries, ",
          nrow(picks), " picks")

  picks$player_id  <- as.character(picks$player_id)
  picks$entry_id   <- as.character(picks$entry_id)
  entries$entry_id <- as.character(entries$entry_id)

  per_player <- aggregate(points ~ player_id, data = scored, FUN = sum)
  appeared   <- unique(scored$player_id)

  joined <- merge(picks[, c("entry_id", "player_id")], per_player,
                  by = "player_id", all.x = TRUE)
  joined$points <- ifelse(is.na(joined$points), 0, joined$points)

  totals <- aggregate(points ~ entry_id, data = joined, FUN = sum)
  names(totals)[names(totals) == "points"] <- "total_points"

  played <- aggregate(player_id ~ entry_id,
                      data = joined[joined$player_id %in% appeared, ],
                      FUN = function(x) length(unique(x)))
  names(played)[names(played) == "player_id"] <- "players_scored"

  out <- merge(entries, totals, by = "entry_id", all.x = TRUE)
  out <- merge(out, played, by = "entry_id", all.x = TRUE)
  out$total_points   <- ifelse(is.na(out$total_points), 0, out$total_points)
  out$players_scored <- ifelse(is.na(out$players_scored), 0L, out$players_scored)

  out[order(-out$total_points, out$entry_name), ]
}

# Per-entry per-player breakdown, for leaderboard drill-down:
# one row per (entry_id, player_id) with that player's total points.
entry_player_points <- function(scored, picks) {
  picks$player_id <- as.character(picks$player_id)
  picks$entry_id  <- as.character(picks$entry_id)

  per_player <- aggregate(points ~ player_id, data = scored, FUN = sum)
  out <- merge(picks[, c("entry_id", "player_id")], per_player,
               by = "player_id", all.x = TRUE)
  out$points <- ifelse(is.na(out$points), 0, out$points)
  out[order(out$entry_id, -out$points), ]
}
