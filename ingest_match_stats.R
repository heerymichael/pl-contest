# ingest_match_stats.R --------------------------------------------------------
# Phase 3 ingestion: populate the player_match_stats tab from TheStatsAPI.
#
# For every FINISHED match not yet ingested (plus any listed in
# REFRESH_MATCH_IDS), this script:
#   1. fetches player-stats (cached to data/api_cache/)
#   2. cross-validates payload goals against the fixture score —
#      mismatches are SKIPPED and flagged for review, never written
#   3. maps API players to contest player_ids via column H of the players tab
#      - unmapped OWNED player  -> match SKIPPED with a loud flag
#        (fix: run map_api_players.R, or hand-map, then re-run this)
#      - unmapped non-owned     -> row dropped, counted in the log
#   4. writes one row per player with minutes > 0
#
# Idempotency: keyed on (match_id, player_id). Re-running is always safe.
# To force re-ingestion of a match (e.g. the API corrected its data), add
# its match_id to REFRESH_MATCH_IDS below — its cache is invalidated and
# its rows replaced.
#
# Manual override columns (own_goals, penalty_saves, shootout_goals,
# shootout_misses, shootout_saves, shootout_result) are initialised to
# 0 / "" and PRESERVED VERBATIM on re-ingestion — fill them by editing the
# Sheet directly. Ingestion never overwrites them.
#
# Raw stats only: no points, no multipliers — those are applied at scoring /
# leaderboard read time, so scoring-rule changes never require re-ingestion.
#
# v2: OWN_GOAL_ALLOWANCE — own goals appear in the fixture score but (most
# likely) in no player's goals count, so they would trip the cross-validation
# gate. When a mismatch flags on a match where you saw an own goal, add
# match_id = <number of own goals> below, re-run, then fill the own_goals
# manual column on the scorer's row in the Sheet.
#
# v3: fixtures list is now fetched LIVE every run (api_call use_cache = FALSE).
# Previously it was disk-cached like everything else and never invalidated, so
# every run after the first re-read a frozen snapshot and never saw newly
# finished matches. Per-match player-stats stay cached (immutable once final;
# refresh via REFRESH_MATCH_IDS).
#
# v4: knockout stage pinning. The API populates stage_name for round_of_16 and
# quarter_final only — it returns NULL for R32, semi-final, third place and
# final, which would fall through to the "group" default and silently score
# those matches at ×1.0. stage_name is now resolved per match_id from
# data/stage_overrides.csv (verified against both the API matchday key and the
# published FIFA schedule), with a hard-stop guard that refuses to ingest any
# finished knockout fixture that isn't pinned. Backfill SF/third/final into the
# CSV when those fixtures enter the feed (the guard will tell you when).
#
# v5: extra-time scores. score$home/away is the 90-MINUTE score only; goals in
# extra time live in score$final_score — which ALSO includes shootout penalty
# goals (verified: mt_836288445 shows 3-4 and mt_019700821 shows 4-5, both 1-1
# matches decided on pens). final_score is therefore only trusted when the
# payload goal total confirms it (i.e. no shootout inflating it). The gate now
# passes on either total; when the final_score branch fires, home_goals /
# away_goals are reassigned so result and team_goals_for/against reflect the
# ET result (W/L, not D). Shootout matches still ingest at the level score
# with result = D — GK win bonuses stay on the manual shootout_result column.
# A match with BOTH ET goals and a shootout matches neither total and
# hard-stops for manual review (correct: it needs eyes).
#
# Run with: source("ingest_match_stats.R")
# ------------------------------------------------------------------------------

message(">>> SOURCED ingest_match_stats.R (v5) at ",
        format(Sys.time(), "%Y-%m-%d %H:%M:%S"))

library(httr)
library(jsonlite)
library(dplyr)

source("R/sheets.R")   # gs_auth(), read_sheet_tab(), sheet_id()

`%||%` <- function(a, b) if (is.null(a)) b else a

# --- Config -------------------------------------------------------------------

BASE_URL   <- "https://api.thestatsapi.com/api/football"
COMP_ID    <- "comp_6107"
SEASON_ID  <- "sn_118868"
CALL_SLEEP <- 0.6
CACHE_DIR  <- "data/api_cache"
STATS_TAB  <- "player_match_stats"

# Force re-ingestion of specific matches (invalidates their cache first):
REFRESH_MATCH_IDS <- c()
# e.g. REFRESH_MATCH_IDS <- c("mt_153637999")

# Own goals per match, to reconcile the cross-validation gate (see header).
# Key = match_id; value = number of own goals in that match. An own goal shows
# in the fixture score but in no player's goals count, so without an entry here
# the match trips the gate and is skipped. After adding a match, also set
# own_goals = 1 on the OG scorer's row in player_match_stats for the -5.
# Format per line:  "match_id" = <count>,   # <SCORE>: <scorer> (<team>) o.g., <min>
OWN_GOAL_ALLOWANCE <- c(
  "mt_517473281" = 1,   # USA 4-1 PAR: Bobadilla (PAR) o.g., 7'
  "mt_641919660" = 1,   # QAT 1-1 SUI: Muheim (SUI) o.g., 90+4'
  "mt_209798581" = 1,   # AUT 3-1 JOR: Al-Arab (JOR) o.g., 76'
  "mt_370102747" = 1,   # BEL 1-1 EGY: Hany (EGY) o.g., 66' (1st of his 2)
  "mt_517473168" = 1,   # IRQ 1-4 NOR: Hussein (IRQ) o.g. (additive to his real goal)
  "mt_894046992" = 1,   # CAN 6-0 QAT: Almanai (QAT) o.g., 75'
  "mt_894046538" = 1,   # AUS 0-2 USA: Burgess (AUS) o.g., 11' — OWNED
  "mt_153637587" = 1,   # ESP 4-0 KSA: Al-Tambakti (KSA) o.g., 49'
  "mt_894046526" = 1,   # POR 5-0 UZB: Nematov (UZB) o.g., 60'
  "mt_517473422" = 1,   # NED 3-1 TUN: Skhiri (TUN) o.g., 3'
  "mt_732520309" = 1,   # BIH 3-1 QAT: Abunada (QAT) o.g., 34'
  "mt_028389482" = 1,   # MAR 4-2 HAI: Bounou (MAR) o.g. (ruled from Lenny Joseph's tap-in)
  "mt_740177225" = 1,   # ARG 3-2 CPV (AET): Borges (CPV) o.g., 2nd half of ET
  "mt_836288496" = 1    # AUS 1-1 EGY (AET, EGY win 4-2 pens): Hany (EGY) o.g., 55' (2nd of his 2)
)

MANUAL_NUM_COLS <- c("own_goals", "penalty_saves", "shootout_goals",
                     "shootout_misses", "shootout_saves")
MANUAL_CHR_COLS <- c("shootout_result")   # "", "W", or "L" (team shootout outcome)

# Knockout stage pinning. match_id -> stage_name for every knockout fixture,
# because the API leaves stage_name NULL for R32 / SF / third place / final
# (only round_of_16 and quarter_final come through populated). Verified against
# the API matchday key AND the published FIFA schedule. The stage_name strings
# here MUST match the keys in STAGE_MULTIPLIERS (R/scoring.R):
#   round_of_32 round_of_16 quarter_final semi_final third_place final
# A third "fixture" column is a human-readable label, ignored by the loader.
STAGE_OVERRIDES <- local({
  f <- "data/stage_overrides.csv"
  if (!file.exists(f)) {
    message(">>> [stage] WARNING — ", f, " missing; no knockout overrides loaded. ",
            "Group-stage runs are unaffected, but the pinning guard will hard-stop ",
            "any finished knockout fixture until this file exists.")
    return(setNames(character(0), character(0)))
  }
  ov <- read.csv(f, stringsAsFactors = FALSE, colClasses = "character")
  ov <- ov[!is.na(ov$match_id) & nzchar(ov$match_id), , drop = FALSE]
  message(">>> [stage] loaded ", nrow(ov), " knockout stage override(s) from ", f)
  setNames(ov$stage_name, ov$match_id)
})

# Resolve a fixture's stage_name: pinned override wins, then the API value,
# then the "group" default. (match() is NA-safe for absent ids; a named-vector
# [[ ]] would error on a miss.)
stage_for <- function(match_id, api_stage) {
  i <- match(match_id, names(STAGE_OVERRIDES))
  if (!is.na(i)) return(unname(STAGE_OVERRIDES[[i]]))
  api_stage %||% "group"
}

dir.create(CACHE_DIR, recursive = TRUE, showWarnings = FALSE)

get_api_key <- function() {
  key <- Sys.getenv("THESTATSAPI_KEY")
  if (nzchar(key)) return(key)
  keyfile <- path.expand("~/.thestatsapi_key")
  if (file.exists(keyfile)) return(trimws(readLines(keyfile, n = 1L)))
  stop("No API key: set THESTATSAPI_KEY or create ~/.thestatsapi_key")
}
API_KEY <- get_api_key()

# --- Cached API wrapper ---------------------------------------------------------

cache_path <- function(endpoint, query = list()) {
  qs <- if (length(query)) {
    paste0("__", paste(names(query), unlist(query), sep = "-", collapse = "_"))
  } else ""
  file.path(CACHE_DIR,
            paste0(gsub("[^a-zA-Z0-9]+", "_", endpoint), qs, ".json"))
}

# use_cache = FALSE bypasses the disk cache entirely (no read, no write) — used
# for the fixtures list, which by definition changes over the tournament. Per-
# match player-stats are immutable once finished, so they keep use_cache = TRUE
# (forced refresh of a corrected match is handled via REFRESH_MATCH_IDS).
api_call <- function(endpoint, query = list(), retry_on_429 = TRUE,
                     use_cache = TRUE) {
  cf <- cache_path(endpoint, query)
  if (use_cache && file.exists(cf)) return(fromJSON(cf, simplifyVector = FALSE))
  
  message(">>> [api] GET ", endpoint,
          if (length(query)) paste0("  [", paste(names(query), unlist(query),
                                                 sep = "=", collapse = ", "), "]") else "")
  resp <- GET(paste0(BASE_URL, endpoint), query = query,
              add_headers(Authorization = paste("Bearer", API_KEY)),
              timeout(30))
  status <- status_code(resp)
  
  if (status == 429L && retry_on_429) {
    message(">>> [api] 429 — sleeping 60s, one retry")
    Sys.sleep(60)
    return(api_call(endpoint, query, retry_on_429 = FALSE, use_cache = use_cache))
  }
  if (status != 200L) {
    message(">>> [api] HTTP ", status, " on ", endpoint, " — skipping (not cached)")
    Sys.sleep(CALL_SLEEP)
    return(NULL)
  }
  
  raw <- content(resp, "text", encoding = "UTF-8")
  if (use_cache) writeLines(raw, cf)
  Sys.sleep(CALL_SLEEP)
  fromJSON(raw, simplifyVector = FALSE)
}

# --- Load Sheet reference data ---------------------------------------------------

message(">>> [sheets] authenticating")
gs_auth()

players <- read_sheet_tab("players") %>%
  mutate(player_id     = as.character(player_id),
         api_player_id = as.character(api_player_id))
message(">>> [sheets] players: ", nrow(players), " rows; mapped api ids: ",
        sum(!is.na(players$api_player_id) & nzchar(players$api_player_id)))

api_to_contest <- players %>%
  filter(!is.na(api_player_id), nzchar(api_player_id)) %>%
  select(api_player_id, player_id, sheet_name = name, sheet_team = team)

picks <- read_sheet_tab("roster_picks")
owned_player_ids <- unique(as.character(picks$player_id))
message(">>> [sheets] roster_picks: ", nrow(picks), " rows; distinct owned players: ",
        length(owned_player_ids))

owned_api_ids <- api_to_contest$api_player_id[
  api_to_contest$player_id %in% owned_player_ids]

teams_tab <- read_sheet_tab("teams")

# Existing ingested data (tab may be empty on first run)
existing <- tryCatch(
  read_sheet_tab(STATS_TAB),
  error = function(e) NULL
)
if (is.null(existing) || nrow(existing) == 0) {
  existing <- data.frame()
  message(">>> [sheets] ", STATS_TAB, " tab empty — first ingestion run")
} else {
  existing <- existing %>% mutate(match_id  = as.character(match_id),
                                  player_id = as.character(player_id))
  message(">>> [sheets] ", STATS_TAB, ": ", nrow(existing), " existing rows across ",
          length(unique(existing$match_id)), " matches")
}

# --- Fetch fixtures, decide what to ingest ----------------------------------------

message(">>> [api] fetching fixtures (all pages)")
fixtures <- list()
page <- 1L
repeat {
  # Fixtures are fetched LIVE every run (use_cache = FALSE): match status and
  # scores change over time, so a cached snapshot would silently freeze the
  # view of which matches have finished.
  fx <- api_call("/matches", query = list(
    competition_id = COMP_ID, season_id = SEASON_ID,
    per_page = 100, page = page
  ), use_cache = FALSE)
  if (is.null(fx)) stop("Fixture fetch failed on page ", page)
  fixtures <- c(fixtures, fx$data)
  tp <- fx$meta$total_pages %||% 1L
  if (page >= tp) break
  page <- page + 1L
}

finished <- Filter(function(m) identical(m$status %||% "", "finished"), fixtures)
already  <- if (nrow(existing) > 0) unique(existing$match_id) else character(0)

todo <- Filter(function(m) {
  !(m$id %in% already) || (m$id %in% REFRESH_MATCH_IDS)
}, finished)

message(">>> [ingest] finished: ", length(finished),
        "; already ingested: ", sum(vapply(finished, function(m)
          m$id %in% already, logical(1))),
        "; to ingest this run: ", length(todo))

# Invalidate cache for forced refreshes
for (mid in REFRESH_MATCH_IDS) {
  cf <- cache_path(paste0("/matches/", mid, "/player-stats"))
  if (file.exists(cf)) {
    file.remove(cf)
    message(">>> [ingest] cache invalidated for refresh: ", mid)
  }
}

# --- Knockout pinning guard ---------------------------------------------------------
# Group fixtures are matchday 1/2/3 with a NULL stage_name (verified across the
# full fixtures payload). ANY finished fixture outside that shape — a matchday
# other than 1/2/3, a missing matchday, or a populated stage_name — is a
# knockout match and MUST be pinned in data/stage_overrides.csv. An unpinned
# knockout would take the "group" default and silently score at ×1.0, and
# because "group" is a KNOWN stage the scoring hard-stop would not catch it.
# So we stop here instead, loudly, before writing anything.
GROUP_MATCHDAYS <- c(1L, 2L, 3L)
is_knockout_fixture <- function(m) {
  md <- m$matchday %||% NA_integer_
  is.na(md) || !(md %in% GROUP_MATCHDAYS) || !is.null(m$stage_name)
}
unpinned_ko <- Filter(function(m) {
  is_knockout_fixture(m) && !(m$id %in% names(STAGE_OVERRIDES))
}, finished)
if (length(unpinned_ko) > 0) {
  details <- vapply(unpinned_ko, function(m) {
    paste0(m$id, "  (matchday ", m$matchday %||% "NA",
           ", api stage_name ", m$stage_name %||% "NULL", ", ",
           m$home_team$name %||% "?", " v ", m$away_team$name %||% "?", ")")
  }, character(1))
  stop("[stage] ", length(unpinned_ko),
       " FINISHED knockout fixture(s) are NOT pinned in data/stage_overrides.csv:\n  ",
       paste(details, collapse = "\n  "),
       "\n  Add each with its correct stage_name (round_of_32 / round_of_16 / ",
       "quarter_final / semi_final / third_place / final) before re-running. ",
       "Ingesting them now would score the match at the group ×1.0 default.")
}

# --- Ingest one match ---------------------------------------------------------------

flatten_player <- function(p) {
  sh <- p$shooting    %||% list()
  pa <- p$passing     %||% list()
  gk <- p$goalkeeping %||% list()
  ge <- p$general     %||% list()
  data.frame(
    api_player_id   = p$player_id      %||% NA_character_,
    api_name        = p$player_name    %||% NA_character_,
    api_team_id     = p$team_id        %||% NA_character_,
    started         = isTRUE(p$started),
    minutes_played  = p$minutes_played %||% 0L,
    goals           = sh$goals           %||% 0L,
    assists         = pa$assists         %||% 0L,
    total_shots     = sh$total_shots     %||% 0L,
    shots_on_target = sh$shots_on_target %||% 0L,
    saves           = gk$saves           %||% 0L,
    yellow_cards    = ge$yellow_cards    %||% 0L,
    red_cards       = ge$red_cards       %||% 0L,
    stringsAsFactors = FALSE
  )
}

ingest_match <- function(m) {
  mid <- m$id
  home_id <- m$home_team$id %||% NA_character_
  away_id <- m$away_team$id %||% NA_character_
  home_goals <- m$score$home %||% NA_integer_
  away_goals <- m$score$away %||% NA_integer_
  # score$final_score includes extra-time goals AND shootout penalty goals
  # (see v5 header note) — only trusted below when the payload total confirms
  # no shootout occurred.
  fin_home   <- m$score$final_score$home %||% NA_integer_
  fin_away   <- m$score$final_score$away %||% NA_integer_
  label <- paste0(m$home_team$name %||% "?", " vs ", m$away_team$name %||% "?")
  
  message(">>> [ingest] ", mid, ": ", label, " (", home_goals, "-", away_goals, ")")
  
  if (is.na(home_goals) || is.na(away_goals)) {
    message(">>> [ingest] SKIP — fixture score missing (API not reconciled yet?)")
    return(NULL)
  }
  
  ps <- api_call(paste0("/matches/", mid, "/player-stats"))
  if (is.null(ps) || length(ps$data %||% list()) == 0) {
    message(">>> [ingest] SKIP — player-stats unavailable or empty")
    return(NULL)
  }
  
  df <- do.call(rbind, lapply(ps$data, flatten_player))
  
  # Cross-validation: payload goals (+ any declared own goals) must equal
  # the fixture score
  payload_goals <- sum(df$goals)
  og_allowance  <- if (mid %in% names(OWN_GOAL_ALLOWANCE)) {
    as.integer(OWN_GOAL_ALLOWANCE[[mid]])
  } else 0L
  score_goals   <- home_goals + away_goals
  final_goals   <- if (!is.na(fin_home) && !is.na(fin_away)) {
    fin_home + fin_away
  } else NA_integer_
  
  if (payload_goals + og_allowance == score_goals) {
    # Decided in 90 minutes, or level after ET (shootout): score$home/away is
    # authoritative. Shootout pens are excluded from the payload, so shootout
    # matches pass here at the level score with result = D — GK win bonuses
    # are handled via the manual shootout_result column.
  } else if (!is.na(final_goals) && payload_goals + og_allowance == final_goals) {
    # Decided by goals in extra time, no shootout (a shootout would inflate
    # final_score above the payload total): final_score is the true match
    # score. Reassign so result and team_goals_for/against reflect the ET
    # outcome (W/L, not D).
    message(">>> [ingest] extra-time result — using final_score ",
            fin_home, "-", fin_away, " (90-min score was ",
            home_goals, "-", away_goals, ")")
    home_goals <- fin_home
    away_goals <- fin_away
  } else {
    message(">>> [ingest] *** MISMATCH — payload goals=", payload_goals,
            if (og_allowance > 0) paste0(" + own-goal allowance=", og_allowance) else "",
            " vs score total=", score_goals,
            " and final_score total=", final_goals,
            " — SKIPPED, review match ", mid, " ***")
    message(">>> [ingest] own goal seen -> add it to OWN_GOAL_ALLOWANCE and re-run; ",
            "a match with ET goals AND a shootout also lands here (review by hand)")
    return(NULL)
  }
  
  # Owned-player safety check: every owned player in this payload must be mapped
  in_payload_owned <- df$api_player_id %in% owned_api_ids
  unmapped <- !(df$api_player_id %in% api_to_contest$api_player_id)
  # Players we can't map AND can't rule out as owned require investigation only
  # if they could be owned. Mapped owned players are fine; the dangerous case is
  # an owned contest player whose api id is missing from column H entirely —
  # they would simply never appear here. Detect that the other way round:
  # owned players from THESE TWO TEAMS with no api mapping at all.
  team_codes <- teams_tab %>%
    filter(!is.na(name)) %>%
    select(name, short_code)
  # api team ids -> contest short codes via the rows we already mapped
  side_codes <- unique(api_to_contest$sheet_team[
    api_to_contest$api_player_id %in% df$api_player_id])
  owned_unmapped <- players %>%
    filter(player_id %in% owned_player_ids,
           team %in% side_codes,
           is.na(api_player_id) | !nzchar(api_player_id))
  if (nrow(owned_unmapped) > 0) {
    message(">>> [ingest] *** OWNED PLAYER UNMAPPED — match SKIPPED ***")
    message(">>> [ingest] run map_api_players.R (or hand-map column H), then re-run:")
    for (i in seq_len(nrow(owned_unmapped))) {
      message("    ", owned_unmapped$name[i], " (", owned_unmapped$team[i],
              ", player_id ", owned_unmapped$player_id[i], ")")
    }
    return(NULL)
  }
  
  n_total <- nrow(df)
  df <- df %>% filter(minutes_played > 0)
  n_played <- nrow(df)
  
  df <- df %>% inner_join(api_to_contest, by = "api_player_id")
  n_dropped <- n_played - nrow(df)
  if (n_dropped > 0) {
    message(">>> [ingest] dropped ", n_dropped,
            " unmapped non-owned player rows (of ", n_played, " who played)")
  }
  
  if (nrow(df) == 0) {
    message(">>> [ingest] SKIP — no mapped players with minutes in this match")
    return(NULL)
  }
  
  df %>%
    mutate(
      match_id           = mid,
      utc_date           = m$utc_date %||% NA_character_,
      stage_name         = stage_for(mid, m$stage_name),
      matchday           = m$matchday %||% NA_integer_,
      group_label        = m$group_label %||% "",
      is_home            = api_team_id == home_id,
      team               = sheet_team,
      opponent           = ifelse(is_home,
                                  m$away_team$name %||% "", m$home_team$name %||% ""),
      team_goals_for     = ifelse(is_home, home_goals, away_goals),
      team_goals_against = ifelse(is_home, away_goals, home_goals),
      result             = ifelse(team_goals_for > team_goals_against, "W",
                                  ifelse(team_goals_for < team_goals_against, "L", "D")),
      own_goals          = 0L,
      penalty_saves      = 0L,
      shootout_goals     = 0L,
      shootout_misses    = 0L,
      shootout_saves     = 0L,
      shootout_result    = "",
      ingested_at        = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    ) %>%
    select(match_id, utc_date, stage_name, matchday, group_label,
           player_id, api_player_id, player_name = sheet_name, team, opponent,
           started, minutes_played, goals, assists, total_shots,
           shots_on_target, saves, yellow_cards, red_cards,
           team_goals_for, team_goals_against, result,
           all_of(MANUAL_NUM_COLS), all_of(MANUAL_CHR_COLS),
           ingested_at)
}

new_rows <- list()
skipped  <- character(0)
for (m in todo) {
  out <- ingest_match(m)
  if (is.null(out)) skipped <- c(skipped, m$id) else new_rows[[m$id]] <- out
}

if (length(new_rows) == 0) {
  message(">>> [ingest] nothing new to write",
          if (length(skipped)) paste0(" (skipped: ",
                                      paste(skipped, collapse = ", "), ")") else "")
  message(">>> [ingest] done at ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
} else {
  
  new_df <- bind_rows(new_rows)
  
  # --- Merge with existing: replace refreshed matches, preserve manual columns ---
  
  if (nrow(existing) > 0) {
    refreshed <- intersect(unique(new_df$match_id), unique(existing$match_id))
    if (length(refreshed) > 0) {
      # carry manual column values across from the old rows
      old_manual <- existing %>%
        filter(match_id %in% refreshed) %>%
        select(match_id, player_id,
               all_of(MANUAL_NUM_COLS), all_of(MANUAL_CHR_COLS))
      new_df <- new_df %>%
        select(-all_of(MANUAL_NUM_COLS), -all_of(MANUAL_CHR_COLS)) %>%
        left_join(old_manual, by = c("match_id", "player_id")) %>%
        mutate(across(all_of(MANUAL_NUM_COLS),
                      ~ ifelse(is.na(.x), 0L, as.integer(.x))),
               across(all_of(MANUAL_CHR_COLS),
                      ~ ifelse(is.na(.x), "", as.character(.x))))
      message(">>> [ingest] refreshed ", length(refreshed),
              " match(es) — manual override columns preserved")
      existing <- existing %>% filter(!(match_id %in% refreshed))
    }
    combined <- bind_rows(existing, new_df)
  } else {
    combined <- new_df
  }
  
  combined <- combined %>% arrange(utc_date, match_id, team, player_name)
  
  message(">>> [sheets] writing ", STATS_TAB, ": ", nrow(combined), " rows (",
          nrow(new_df), " new/refreshed across ", length(new_rows), " match(es))")
  googlesheets4::sheet_write(combined, ss = sheet_id(), sheet = STATS_TAB)
  message(">>> [sheets] write complete")
  
  if (length(skipped) > 0) {
    message(">>> [ingest] SKIPPED matches needing attention: ",
            paste(skipped, collapse = ", "))
  }
  message(">>> [ingest] done at ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
}