# validate_player_stats.R ----------------------------------------------------
# Purpose: validate the captured TheStatsAPI player-stats payloads against the
# contest's scoring inputs, cross-checked against known real results:
#   - Mexico 2-0 South Africa (three red cards)
#   - South Korea 2-1 Czechia
#
# Reads:  data/api_samples/player_stats_mexico_vs_south_africa.json
#         data/api_samples/player_stats_south_korea_vs_czechia.json
# Writes: nothing (console report only)
#
# Run with: source("validate_player_stats.R")
# ----------------------------------------------------------------------------

message(">>> SOURCED validate_player_stats.R (v1) at ",
        format(Sys.time(), "%Y-%m-%d %H:%M:%S"))

library(jsonlite)

`%||%` <- function(a, b) if (is.null(a)) b else a

FILES <- c(
  "Mexico 2-0 South Africa" =
    "data/api_samples/player_stats_mexico_vs_south_africa.json",
  "South Korea 2-1 Czechia" =
    "data/api_samples/player_stats_south_korea_vs_czechia.json"
)

# Expected truths from the real matches (for cross-checking)
EXPECTED <- list(
  "Mexico 2-0 South Africa" = list(goals = 2, red_cards = 3),
  "South Korea 2-1 Czechia" = list(goals = 3, red_cards = NA) # reds unknown; report only
)

# --- Flatten one player object to the fields scoring needs -------------------

flatten_player <- function(p) {
  sh <- p$shooting    %||% list()
  pa <- p$passing     %||% list()
  gk <- p$goalkeeping %||% list()
  ge <- p$general     %||% list()
  data.frame(
    player_id       = p$player_id        %||% NA_character_,
    player_name     = p$player_name      %||% NA_character_,
    team_id         = p$team_id          %||% NA_character_,
    position        = p$position         %||% NA_character_,
    started         = p$started          %||% NA,
    played          = p$played           %||% NA,
    minutes_played  = p$minutes_played   %||% NA_integer_,
    goals           = sh$goals           %||% NA_integer_,
    total_shots     = sh$total_shots     %||% NA_integer_,
    shots_on_target = sh$shots_on_target %||% NA_integer_,
    assists         = pa$assists         %||% NA_integer_,
    saves           = gk$saves           %||% NA_integer_,
    yellow_cards    = ge$yellow_cards    %||% NA_integer_,
    red_cards       = ge$red_cards       %||% NA_integer_,
    subbed_on       = ge$player_subbed_on  %||% NA,
    subbed_off      = ge$player_subbed_off %||% NA,
    stringsAsFactors = FALSE
  )
}

sum_na0 <- function(x) sum(ifelse(is.na(x), 0L, x))

pct_present <- function(x) {
  if (is.character(x)) {
    round(100 * mean(!is.na(x) & nzchar(x)), 1)
  } else {
    round(100 * mean(!is.na(x)), 1)
  }
}

check <- function(label, ok, detail = "") {
  message(sprintf("    [%s] %s%s",
                  if (isTRUE(ok)) "PASS" else if (is.na(ok)) "INFO" else "FAIL",
                  label,
                  if (nzchar(detail)) paste0(" — ", detail) else ""))
}

# --- Validate each file -------------------------------------------------------

for (match_name in names(FILES)) {
  path <- FILES[[match_name]]
  message("\n>>> ============ ", match_name, " ============")
  
  if (!file.exists(path)) {
    message(">>> MISSING FILE: ", path, " — skipping")
    next
  }
  
  payload <- fromJSON(path, simplifyVector = FALSE)
  players <- payload$data %||% list()
  message(">>> rows in payload: ", length(players))
  
  df <- do.call(rbind, lapply(players, flatten_player))
  
  exp <- EXPECTED[[match_name]]
  
  message(">>> --- Scoring input coverage (% of rows populated) ---")
  for (col in c("position", "minutes_played", "goals", "total_shots",
                "shots_on_target", "assists", "saves",
                "yellow_cards", "red_cards")) {
    message(sprintf("    %-16s %5.1f%%", col, pct_present(df[[col]])))
  }
  
  message(">>> --- Cross-checks against known result ---")
  
  total_goals <- sum_na0(df$goals)
  check("total goals", total_goals == exp$goals,
        sprintf("payload=%d expected=%d", total_goals, exp$goals))
  
  total_reds <- sum_na0(df$red_cards)
  if (is.na(exp$red_cards)) {
    check("total red cards", NA, sprintf("payload=%d (no expectation set)", total_reds))
  } else {
    check("total red cards", total_reds == exp$red_cards,
          sprintf("payload=%d expected=%d", total_reds, exp$red_cards))
  }
  
  total_yellows <- sum_na0(df$yellow_cards)
  check("yellow cards present", NA, sprintf("payload total=%d", total_yellows))
  
  total_assists <- sum_na0(df$assists)
  check("assists vs goals sanity", total_assists <= total_goals,
        sprintf("assists=%d goals=%d (assists should be <= goals)",
                total_assists, total_goals))
  
  gk_rows <- df[!is.na(df$saves) & df$saves > 0, , drop = FALSE]
  check("GK saves attributed", nrow(gk_rows) >= 1,
        if (nrow(gk_rows) > 0)
          paste(sprintf("%s: %d", gk_rows$player_name, gk_rows$saves),
                collapse = "; ")
        else "no rows with saves > 0")
  
  # Minutes: starters who finished should be ~90+; check plausibility
  played_rows <- df[!is.na(df$minutes_played) & df$minutes_played > 0, ,
                    drop = FALSE]
  check("minutes plausible", NA,
        sprintf("rows with minutes>0: %d; max=%s; rows >=60min: %d",
                nrow(played_rows),
                if (nrow(played_rows)) max(played_rows$minutes_played) else "-",
                sum(played_rows$minutes_played >= 60)))
  
  blank_pos <- df[is.na(df$position) | !nzchar(df$position), , drop = FALSE]
  check("blank positions (gotcha 1)", nrow(blank_pos) == 0,
        sprintf("%d of %d rows blank%s", nrow(blank_pos), nrow(df),
                if (nrow(blank_pos) > 0)
                  paste0(": ", paste(head(blank_pos$player_name, 8),
                                     collapse = ", "))
                else ""))
  
  message(">>> --- Event rows (goals / cards) for eyeball check ---")
  ev <- df[(!is.na(df$goals) & df$goals > 0) |
             (!is.na(df$red_cards) & df$red_cards > 0) |
             (!is.na(df$yellow_cards) & df$yellow_cards > 0), ,
           drop = FALSE]
  if (nrow(ev) > 0) {
    print(ev[, c("player_name", "team_id", "position", "minutes_played",
                 "goals", "assists", "yellow_cards", "red_cards")],
          row.names = FALSE)
  } else {
    message("    (none — that would be a problem)")
  }
}

# --- Own-goal field hunt ------------------------------------------------------
# Scoring needs own goals (-5). The field notes never mention an own-goal
# field, so scan every key name in both payloads for candidates.

message("\n>>> ============ Own-goal / penalty field hunt ============")
all_keys <- character(0)
collect_keys <- function(x, prefix = "") {
  if (is.list(x) && !is.null(names(x))) {
    for (nm in names(x)) {
      all_keys <<- c(all_keys, paste0(prefix, nm))
      collect_keys(x[[nm]], paste0(prefix, nm, "$"))
    }
  } else if (is.list(x) && length(x) > 0) {
    collect_keys(x[[1]], prefix)  # sample first element of arrays
  }
}
for (path in FILES) {
  if (file.exists(path)) collect_keys(fromJSON(path, simplifyVector = FALSE))
}
all_keys <- sort(unique(all_keys))
hits <- grep("own|pen", all_keys, ignore.case = TRUE, value = TRUE)
if (length(hits) > 0) {
  message(">>> candidate fields: ", paste(hits, collapse = ", "))
} else {
  message(">>> NO own-goal or penalty fields found in player-stats — ",
          "these will need the timeline endpoint (gotcha 6)")
}

message("\n>>> [validate] full key inventory (", length(all_keys), " keys):")
message(paste(all_keys, collapse = ", "))

message("\n>>> [validate] done at ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))