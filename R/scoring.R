# R/scoring.R ------------------------------------------------------------------
# Pure scoring engine for the World Cup 2026 Challenge.
#
# No side effects, no Sheets reads, no caching — callers (leaderboard module,
# console checks) supply the data frames and get points back. Because nothing
# is precomputed or stored, a scoring correction here fixes all history the
# moment the leaderboard re-renders.
#
# Implements the published rules in nav.R rules_view():
#   All positions (including GK, per commissioner ruling 12 Jun 2026):
#     goal +10, assist +6, shot +1, shot on target +1
#     (the API's shot counts include goals, so each goal nets 12 as published)
#     yellow −1.5, red −5; a red charges at most ONE yellow, so any
#     yellow+red combination lands on −6.5 as published
#     own goal −5 (manual column)
#     shootout: goal +1.5, miss −1 (manual columns)
#   Clean sheets (team concedes 0 across the entire match AND the player
#   played MORE than 60 minutes): GK +8, DEF +6, MID +4, FWD 0
#   GK only:
#     save +2, win +5 (shootout wins count via shootout_result == "W"),
#     penalty save (in play) +3 (manual column), shootout save +1.5 (manual),
#     goal conceded −2 (team-level: every GK row in the match is charged the
#     full match's conceded — commissioner-accepted simplification, manual
#     adjustment if a mid-match GK substitution ever makes it matter)
#   Round multipliers (published rules): group ×1.0, R32 ×1.2, R16 ×1.4,
#   QF ×1.6, SF ×1.8, third-place playoff ×0.0 (does not score), final ×2.0.
#   Any stage_name NOT in STAGE_MULTIPLIERS HARD-STOPS scoring rather than
#   silently defaulting — a wrong knockout multiplier would publish wrong
#   leaderboards. The stage_name itself is pinned per match_id at ingestion
#   (data/stage_overrides.csv) because the API leaves it NULL for R32, SF,
#   third place and final.
#
# Main entry points:
#   score_player_match_stats(stats, players)  -> stats + points columns
#   score_entries(scored, picks, entries)     -> per-entry totals
#   entry_player_points(scored, picks)        -> per-entry per-player breakdown
# ------------------------------------------------------------------------------

# Stage multipliers per the published rules. stage_name strings are pinned at
# ingestion via data/stage_overrides.csv (the API returns NULL for R32, SF,
# third place and final), so these keys are the canonical knockout names.
# third_place is 0.0 by commissioner ruling (the playoff does not score).
STAGE_MULTIPLIERS <- c(
  "group"         = 1.0,
  "round_of_32"   = 1.2,
  "round_of_16"   = 1.4,
  "quarter_final" = 1.6,
  "semi_final"    = 1.8,
  "third_place"   = 0.0,
  "final"         = 2.0
)

wc_round_multiplier <- function(stage_name) {
  s <- tolower(trimws(as.character(stage_name)))
  s[is.na(s) | s == ""] <- "group"
  unknown <- setdiff(unique(s), names(STAGE_MULTIPLIERS))
  if (length(unknown) > 0) {
    stop("wc_round_multiplier: unrecognised stage_name(s): ",
         paste(unknown, collapse = ", "),
         " — add them to STAGE_MULTIPLIERS in R/scoring.R before scoring ",
         "these matches (knockout multipliers must be explicit, never assumed)")
  }
  unname(STAGE_MULTIPLIERS[s])
}

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
# Returns stats joined with position plus: base_points, multiplier, points.
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
  saves           <- .num0(df$saves)
  yellows         <- .num0(df$yellow_cards)
  reds            <- .num0(df$red_cards)
  minutes         <- .num0(df$minutes_played)
  conceded        <- .num0(df$team_goals_against)
  own_goals       <- .num0(df$own_goals)
  pen_saves       <- .num0(df$penalty_saves)
  so_goals        <- .num0(df$shootout_goals)
  so_misses       <- .num0(df$shootout_misses)
  so_saves        <- .num0(df$shootout_saves)
  
  is_gk  <- df$position == "GK"
  result <- as.character(df$result)
  so_res <- toupper(trimws(ifelse(is.na(df$shootout_result), "",
                                  as.character(df$shootout_result))))
  is_win <- result == "W" | so_res == "W"
  
  # Standard scoring — all positions, including GK
  attacking <- goals * 10 + assists * 6 + total_shots * 1 + shots_on_target * 1
  
  # Cards: a red charges at most one yellow (second yellow is scored AS the
  # red, added to the first yellow: -1.5 - 5 = -6.5 per published rules)
  yellows_charged <- ifelse(reds > 0, pmin(yellows, 1), yellows)
  cards <- yellows_charged * -1.5 + reds * -5
  
  # Clean sheets: team-level zero across the whole match, strictly >60 minutes
  cs_eligible <- conceded == 0 & minutes > 60
  cs_value <- c(GK = 8, DEF = 6, MID = 4, FWD = 0)[df$position]
  clean_sheet <- ifelse(cs_eligible, cs_value, 0)
  
  # GK-specific
  gk_points <- ifelse(is_gk,
                      saves * 2 +
                        ifelse(is_win, 5, 0) +
                        pen_saves * 3 +
                        conceded * -2,
                      0)
  
  # Manual-column events
  manual <- own_goals * -5 + so_goals * 1.5 + so_misses * -1 + so_saves * 1.5
  
  df$base_points <- attacking + cards + clean_sheet + gk_points + manual
  df$multiplier  <- wc_round_multiplier(df$stage_name)
  df$points      <- df$base_points * df$multiplier
  
  message(">>> score_player_match_stats: scored ", nrow(df), " rows, ",
          "total points in play ", round(sum(df$points), 1))
  df
}

# --- Per-entry aggregation -------------------------------------------------------

# scored:  output of score_player_match_stats()
# picks:   roster_picks rows (entry_id, player_id)
# entries: entries rows (entry_id, entry_name, display_name, ...)
#
# Returns one row per entry with total_points, players_scored (distinct
# rostered players who have at least one scored row), sorted descending.
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