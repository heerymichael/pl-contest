# ingest_match_stats.R -------------------------------------------------------
#
# Pulls finished-match player statistics from TheStatsAPI and appends them to
# the `player_match_stats` tab on pl-contest-data. That tab is the sole input
# to scoring: R/scoring.R turns it into points, R/gameweeks.R gives each row a
# gameweek, R/bestball.R reduces it to each entry's optimal XI.
#
# EDIT DRY_RUN IN THIS FILE, not at the console (console assignment is
# overwritten on source()).
#
# Run after:  source("R/sheets.R"); gs_auth()
#
# --- DESIGN NOTES -----------------------------------------------------------
#
# RESOLUTION IS DEFERRED, NOT DONE HERE. Every row carries `api_player_id` as
# its durable key, plus a best-effort `player_id` resolved against the players
# tab at ingest time. Rows whose player is not yet mapped are STILL WRITTEN,
# with player_id blank. The app re-resolves from api_player_id on every render
# (resolve_player_ids() in R/scoring.R), so filling a mapping gap later makes
# those points appear retroactively across all history with no re-scrape.
# Dropping unmapped rows here instead would make every mapping gap permanent.
#
# OWN GOALS come only from /shotmap — they never appear in the player payload
# (the scorer's `shooting.goals` stays 0). The shotmap credits an own goal to
# the BENEFITING team: the row reads team_name = the opponent, and the
# `goalkeeper` is the scorer's own keeper. So the own goal is charged to
# `player_id`, and that player's actual club is taken from the player-stats
# payload, never from the shotmap row. Getting this backwards charges every
# own goal to a player on the wrong team.
#
# PENALTY SAVES also come from /shotmap: is_penalty && result == "save",
# credited to goalkeeper$id. In-play only — league matches have no shootouts.
#
# GOAL RECONCILIATION is a hard gate. For each match, each side's goals are
# recomputed as (that side's players' goals) + (own goals scored by the other
# side) and compared to the official score. A mismatch means the payload and
# the scoreline disagree, so the match is REFUSED and reported rather than
# written half-right. This is the check that catches a silently changed API
# shape before it reaches the leaderboard.
#
# GAMEWEEK IS NOT STORED. The official `matchday` is written for reference,
# but the contest gameweek is derived from utc_date against the gameweeks tab
# at render time — so a rearranged fixture lands in the right gameweek
# automatically and a boundary correction needs no re-ingest.
#
# APPEND ONLY. Matches already present are skipped unless FORCE_REINGEST.
# Nothing is ever rewritten, so there is no non-atomic read-modify-write
# window on a tab that will grow to thousands of rows.
# ---------------------------------------------------------------------------

DRY_RUN <- TRUE

# Which matches. Leave both NULL for the normal run: every finished match in
# the season that isn't on the tab yet.
ONLY_GAMEWEEK  <- NULL    # e.g. 1L — restrict to one contest gameweek
ONLY_MATCH_IDS <- NULL    # e.g. c("mt_511976372") — overrides everything

FORCE_REINGEST <- FALSE   # TRUE = re-fetch matches already on the tab
# (still appends; de-duplicate manually afterwards)

# Raw payloads are kept OUTSIDE the repo: they are large, they must never
# reach a shinyapps.io bundle, and they are the only way to re-derive a
# scoring change without re-hitting the API. Set to NA to skip saving.
RAW_DIR <- path.expand("~/Desktop/football-data/pl2627_raw")

MAX_MATCHES <- 20L        # per-run ceiling, so a mistake costs one gameweek
# of API calls rather than a season's

suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
  library(googlesheets4)
  library(dplyr)
})

message(">>> SOURCED ingest_match_stats.R at ", format(Sys.time()),
        if (DRY_RUN) "  [DRY RUN]" else "  [LIVE WRITE]")

# --- Config -----------------------------------------------------------------

STATS_API_BASE    <- "https://api.thestatsapi.com/api"
PL_COMPETITION_ID <- "comp_3039"
PL_SEASON_ID      <- "sn_8406098"    # 26/27, confirmed by probe_pl_2627.R
STATS_TAB         <- "player_match_stats"
API_PAUSE         <- 0.6             # seconds between calls

SHEET <- if (exists("SHEET_ID")) get("SHEET_ID") else
  "1oFdL_U-U4G-3YGXd13Pcb6s4-U01reZwEeVQ-ceUdI8"

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# --- API --------------------------------------------------------------------

stats_api_key <- function() {
  key <- Sys.getenv("THESTATSAPI_KEY")
  if (!nzchar(key)) {
    stop("THESTATSAPI_KEY not set. Add it to .Renviron and restart R.",
         call. = FALSE)
  }
  key
}

api_get <- function(path, params = list(), save_as = NA) {
  Sys.sleep(API_PAUSE)
  resp <- tryCatch(
    request(STATS_API_BASE) |>
      req_url_path_append(path) |>
      req_url_query(!!!params) |>
      req_auth_bearer_token(stats_api_key()) |>
      req_error(is_error = function(r) FALSE) |>
      req_perform(),
    error = function(e) {
      message("    [api] TRANSPORT FAILURE on ", path, ": ",
              conditionMessage(e))
      NULL
    }
  )
  if (is.null(resp)) return(list(ok = FALSE, status = NA_integer_, data = NULL))
  
  status <- resp_status(resp)
  body   <- tryCatch(resp_body_string(resp), error = function(e) "")
  
  if (!is.na(save_as) && !is.na(RAW_DIR) && nzchar(body) && status < 400) {
    dir.create(RAW_DIR, showWarnings = FALSE, recursive = TRUE)
    writeLines(body, file.path(RAW_DIR, paste0(save_as, ".json")))
  }
  
  if (status >= 400) {
    message("    [api] HTTP ", status, " on ", path)
    return(list(ok = FALSE, status = status, data = NULL))
  }
  list(ok = TRUE, status = status,
       data = tryCatch(fromJSON(body, simplifyVector = FALSE),
                       error = function(e) NULL))
}

# --- Small helpers ----------------------------------------------------------

# Safe nested extraction: gv(x, "shooting", "goals")
gv <- function(x, ...) {
  for (k in c(...)) {
    if (is.null(x)) return(NULL)
    x <- x[[k]]
  }
  x
}

# Numeric coercion that never returns NA or length != 1
n0 <- function(x) {
  if (is.null(x) || length(x) == 0) return(0)
  v <- suppressWarnings(as.numeric(unname(x)[1]))
  if (is.na(v)) 0 else v
}

chr1 <- function(x, default = NA_character_) {
  if (is.null(x) || length(x) == 0) return(default)
  as.character(x[[1]])
}

blank <- function(x) {
  x <- trimws(as.character(x))
  is.na(x) | !nzchar(x) | toupper(x) %in% c("NA", "NULL")
}

# --- Reference data ---------------------------------------------------------

message(">>> [ingest] reading reference tabs")

players_tab <- range_read(SHEET, sheet = "players", col_types = "c")
teams_tab   <- range_read(SHEET, sheet = "teams",   col_types = "c")

players_tab <- players_tab |>
  mutate(player_id     = as.character(player_id),
         api_player_id = trimws(as.character(api_player_id)))

api_to_pid <- players_tab |>
  filter(!blank(api_player_id)) |>
  distinct(api_player_id, .keep_all = TRUE)
api_to_pid <- setNames(api_to_pid$player_id, api_to_pid$api_player_id)

# Club short codes. The API's team name matches teams.api_name.
name_to_code <- setNames(as.character(teams_tab$short_code),
                         as.character(teams_tab$api_name))

message("    [ingest] players ", nrow(players_tab), " (",
        length(api_to_pid), " with api_player_id) | teams ", nrow(teams_tab))

# Existing stats — for the already-ingested check and the append target.
existing <- tryCatch(range_read(SHEET, sheet = STATS_TAB, col_types = "c"),
                     error = function(e) NULL)
tab_exists <- !is.null(existing)
done_ids <- if (tab_exists && nrow(existing) > 0)
  unique(as.character(existing$match_id)) else character(0)

message("    [ingest] ", STATS_TAB, ": ",
        if (!tab_exists) "tab does not exist yet — will be created"
        else paste0(nrow(existing), " rows across ", length(done_ids),
                    " match(es) already ingested"))

# --- Fixture list -----------------------------------------------------------

message(">>> [ingest] fetching season fixtures")

# The API only exposes fixtures loaded so far (MD1-10 as of 11 Oct 2026, a
# rolling horizon), and meta$total_pages is authoritative. Stop on it rather
# than on "page shorter than 100": a page that is both full AND last (total
# exactly 100) otherwise triggers a request for page 2, which the API
# rejects with HTTP 400 PAGE_OUT_OF_RANGE and used to abort the ingest.
fixtures <- list()
total_pages <- NA_integer_
for (pg in 1:5) {
  r <- api_get("football/matches",
               params = list(competition_id = PL_COMPETITION_ID,
                             season_id      = PL_SEASON_ID,
                             per_page       = 100,
                             page           = pg))
  if (!isTRUE(r$ok)) stop("Fixture fetch failed on page ", pg, call. = FALSE)
  page <- r$data$data %||% list()
  fixtures <- c(fixtures, page)
  total_pages <- suppressWarnings(as.integer(r$data$meta$total_pages %||% NA))
  if (!is.na(total_pages) && pg >= total_pages) break
  if (is.na(total_pages) && length(page) < 100) break
}
message("    [ingest] ", length(fixtures), " fixtures in season ",
        PL_SEASON_ID, " (", if (is.na(total_pages)) "?" else total_pages,
        " page(s); API horizon up to matchday ",
        suppressWarnings(max(vapply(fixtures, function(f) n0(f$matchday), numeric(1)),
                             na.rm = TRUE)), ")")

fx <- do.call(rbind, lapply(fixtures, function(f) data.frame(
  match_id  = chr1(f$id),
  matchday  = n0(f$matchday),
  status    = chr1(f$status),
  utc_date  = chr1(f$utc_date),
  home_name = chr1(gv(f, "home_team", "name")),
  away_name = chr1(gv(f, "away_team", "name")),
  home_id   = chr1(gv(f, "home_team", "id")),
  away_id   = chr1(gv(f, "away_team", "id")),
  home_goals = if (is.null(gv(f, "score", "home"))) NA_real_
  else n0(gv(f, "score", "home")),
  away_goals = if (is.null(gv(f, "score", "away"))) NA_real_
  else n0(gv(f, "score", "away")),
  stringsAsFactors = FALSE
)))

finished <- fx |> filter(tolower(status) == "finished",
                         !is.na(home_goals), !is.na(away_goals))
message("    [ingest] finished with a score: ", nrow(finished))

# Contest gameweek, for the ONLY_GAMEWEEK filter and the console summary.
if (exists("gameweek_of_kickoff")) {
  finished$gameweek <- gameweek_of_kickoff(finished$utc_date)
} else {
  message("    [ingest] note: R/gameweeks.R not sourced — gameweek column ",
          "left blank in the console summary (does not affect the write)")
  finished$gameweek <- NA_integer_
}

targets <- finished
if (!is.null(ONLY_MATCH_IDS)) {
  targets <- targets |> filter(match_id %in% ONLY_MATCH_IDS)
} else {
  if (!is.null(ONLY_GAMEWEEK)) {
    targets <- targets |> filter(!is.na(gameweek), gameweek %in% ONLY_GAMEWEEK)
  }
  if (!FORCE_REINGEST) targets <- targets |> filter(!match_id %in% done_ids)
}

message("")
message(">>> [ingest] ", nrow(targets), " match(es) to ingest")
if (nrow(targets) == 0) {
  message(">>> [ingest] nothing to do.")
} else {
  print(targets |>
          mutate(fixture = paste0(home_name, " ", home_goals, "-",
                                  away_goals, " ", away_name)) |>
          select(match_id, gameweek, matchday, utc_date, fixture) |>
          as.data.frame(), row.names = FALSE)
}

if (nrow(targets) > MAX_MATCHES) {
  stop("Refusing to fetch ", nrow(targets), " matches in one run ",
       "(MAX_MATCHES = ", MAX_MATCHES, "). Narrow with ONLY_GAMEWEEK or ",
       "raise the ceiling deliberately.", call. = FALSE)
}

# --- Per-match build --------------------------------------------------------

build_match_rows <- function(fxr) {
  mid <- fxr$match_id
  message("")
  message(">>> [ingest] ", mid, "  ", fxr$home_name, " ", fxr$home_goals,
          "-", fxr$away_goals, " ", fxr$away_name,
          "  (MD", fxr$matchday,
          if (!is.na(fxr$gameweek)) paste0(" / GW", fxr$gameweek) else "", ")")
  
  ps <- api_get(paste0("football/matches/", mid, "/player-stats"),
                save_as = paste0(mid, "_player_stats"))
  if (!isTRUE(ps$ok)) return(list(ok = FALSE, why = "player-stats fetch failed"))
  rows_in <- ps$data$data %||% list()
  if (length(rows_in) == 0) return(list(ok = FALSE, why = "empty player-stats"))
  
  sm <- api_get(paste0("football/matches/", mid, "/shotmap"),
                save_as = paste0(mid, "_shotmap"))
  shots <- if (isTRUE(sm$ok)) (sm$data$data %||% list()) else list()
  if (!isTRUE(sm$ok)) {
    message("    [ingest] !!! shotmap unavailable — own goals and penalty ",
            "saves cannot be detected for this match")
  }
  
  # Club of every player in this match, from the AUTHORITATIVE payload.
  team_of <- setNames(
    vapply(rows_in, function(p) chr1(p$team_id), character(1)),
    vapply(rows_in, function(p) chr1(p$player_id), character(1))
  )
  
  # Own goals: charged to the scorer. Their club comes from team_of, NOT
  # from the shotmap row (which names the benefiting team).
  # Accumulated as id vectors then tabled: `l[[key]]` on a list errors on
  # a key that isn't there yet, so incrementing in place is a trap.
  og_ids <- character(0)
  ps_ids <- character(0)
  for (s in shots) {
    if (identical(chr1(s$goal_type), "own")) {
      og_ids <- c(og_ids, chr1(s$player_id))
    }
    if (isTRUE(s$is_penalty) && identical(chr1(s$result), "save")) {
      gid <- chr1(gv(s, "goalkeeper", "id"))
      if (!is.na(gid)) ps_ids <- c(ps_ids, gid)
    }
  }
  og       <- table(og_ids)
  ps_saves <- table(ps_ids)
  
  if (length(og) > 0) {
    message("    [ingest] own goal(s): ",
            paste(names(og), as.integer(og), sep = " x", collapse = ", "))
  }
  if (length(ps_saves) > 0) {
    message("    [ingest] penalty save(s): ",
            paste(names(ps_saves), as.integer(ps_saves), sep = " x",
                  collapse = ", "))
  }
  
  home_code <- unname(name_to_code[fxr$home_name])
  away_code <- unname(name_to_code[fxr$away_name])
  if (is.na(home_code) || is.na(away_code)) {
    return(list(ok = FALSE,
                why = paste0("club name not in teams.api_name: ",
                             if (is.na(home_code)) fxr$home_name else "",
                             if (is.na(away_code)) fxr$away_name else "")))
  }
  
  out <- lapply(rows_in, function(p) {
    apid <- chr1(p$player_id)
    tid  <- chr1(p$team_id)
    is_home <- identical(tid, fxr$home_id)
    
    # A player whose team_id matches neither side is a payload fault, not
    # something to guess at.
    if (!is_home && !identical(tid, fxr$away_id)) return(NULL)
    
    gf <- if (is_home) fxr$home_goals else fxr$away_goals
    ga <- if (is_home) fxr$away_goals else fxr$home_goals
    
    data.frame(
      match_id            = mid,
      utc_date            = fxr$utc_date,
      matchday            = fxr$matchday,
      team                = if (is_home) home_code else away_code,
      opponent            = if (is_home) away_code else home_code,
      is_home             = is_home,
      api_player_id       = apid,
      player_id           = unname(api_to_pid[apid]) %||% NA_character_,
      player_name         = chr1(p$player_name),
      api_position        = chr1(p$position),
      minutes_played      = n0(p$minutes_played),
      goals               = n0(gv(p, "shooting", "goals")),
      assists             = n0(gv(p, "passing",  "assists")),
      total_shots         = n0(gv(p, "shooting", "total_shots")),
      shots_on_target     = n0(gv(p, "shooting", "shots_on_target")),
      tackles             = n0(gv(p, "defending", "tackles")),
      interceptions       = n0(gv(p, "defending", "interceptions")),
      saves               = n0(gv(p, "goalkeeping", "saves")),
      penalty_saves       = n0(ps_saves[apid]),
      own_goals           = n0(og[apid]),
      yellow_cards        = n0(gv(p, "general", "yellow_cards")),
      red_cards           = n0(gv(p, "general", "red_cards")),
      team_goals_for      = gf,
      team_goals_against  = ga,
      result              = if (gf > ga) "W" else if (gf == ga) "D" else "L",
      played              = isTRUE(p$played),
      stringsAsFactors    = FALSE
    )
  })
  out <- do.call(rbind, Filter(Negate(is.null), out))
  if (is.null(out) || nrow(out) == 0) {
    return(list(ok = FALSE, why = "no rows survived team matching"))
  }
  
  # --- Reconciliation gate ---------------------------------------------
  # Each side's goals = its players' goals + own goals scored by the other
  # side. `team_of` gives the scorer's real club, so an own goal is added
  # to the OPPOSITE side's tally.
  og_for <- c(0, 0); names(og_for) <- c(home_code, away_code)
  for (pid in names(og)) {
    scorer_team <- unname(team_of[pid])
    credited <- if (identical(scorer_team, fxr$home_id)) away_code else
      if (identical(scorer_team, fxr$away_id)) home_code else NA_character_
    if (is.na(credited)) {
      return(list(ok = FALSE,
                  why = paste0("own goal by ", pid,
                               " whose club is not in this match")))
    }
    og_for[credited] <- og_for[credited] + as.integer(og[[pid]])
  }
  
  calc_home <- sum(out$goals[out$team == home_code]) + og_for[[home_code]]
  calc_away <- sum(out$goals[out$team == away_code]) + og_for[[away_code]]
  
  if (calc_home != fxr$home_goals || calc_away != fxr$away_goals) {
    return(list(ok = FALSE, why = paste0(
      "goal reconciliation FAILED: payload gives ", calc_home, "-", calc_away,
      " (incl. own goals ", og_for[[home_code]], "/", og_for[[away_code]],
      "), official score is ", fxr$home_goals, "-", fxr$away_goals)))
  }
  message("    [ingest] reconciled ", calc_home, "-", calc_away, " \u2713")
  
  # Unused subs score nothing and would only bloat the tab.
  keep <- out$played | out$minutes_played > 0
  message("    [ingest] ", sum(keep), " players used (",
          sum(!keep), " unused subs dropped)")
  out <- out[keep, , drop = FALSE]
  out$played <- NULL
  
  unmapped <- out$api_player_id[is.na(out$player_id)]
  if (length(unmapped) > 0) {
    message("    [ingest] ", length(unmapped), " row(s) unmapped to a ",
            "contest player — WRITTEN ANYWAY, will resolve when ",
            "api_player_id is filled: ",
            paste(utils::head(out$player_name[is.na(out$player_id)], 6),
                  collapse = ", "))
  }
  
  list(ok = TRUE, rows = out)
}

# --- Run --------------------------------------------------------------------

collected <- list()
refused   <- character(0)

if (nrow(targets) > 0) {
  for (i in seq_len(nrow(targets))) {
    res <- build_match_rows(targets[i, ])
    if (isTRUE(res$ok)) {
      collected[[length(collected) + 1L]] <- res$rows
    } else {
      refused <- c(refused, paste0(targets$match_id[i], ": ", res$why))
      message("    [ingest] !!! REFUSED — ", res$why)
    }
  }
}

new_rows <- if (length(collected) > 0) do.call(rbind, collected) else NULL

message("")
message(">>> [ingest] ================ SUMMARY ================")
message(">>> [ingest] matches ingested : ", length(collected),
        " of ", nrow(targets))
message(">>> [ingest] rows built       : ",
        if (is.null(new_rows)) 0 else nrow(new_rows))
if (!is.null(new_rows)) {
  message(">>> [ingest] mapped rows      : ", sum(!is.na(new_rows$player_id)),
          " | unmapped: ", sum(is.na(new_rows$player_id)))
  message(">>> [ingest] goals / assists  : ", sum(new_rows$goals), " / ",
          sum(new_rows$assists),
          " | own goals: ", sum(new_rows$own_goals),
          " | pen saves: ", sum(new_rows$penalty_saves))
}
if (length(refused) > 0) {
  message(">>> [ingest] !!! ", length(refused), " match(es) REFUSED:")
  for (r in refused) message("      ", r)
}

# --- Write ------------------------------------------------------------------

new_rows_out <- new_rows
if (!is.null(new_rows_out)) {
  new_rows_out$ingested_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S",
                                     tz = "Europe/London")
  # Everything as character: player_id must never round-trip through a
  # numeric type, and the app reads this tab with col_types = "c".
  new_rows_out[] <- lapply(new_rows_out, function(col) {
    out <- as.character(col)
    ifelse(is.na(out), "", out)
  })
}

if (is.null(new_rows_out) || nrow(new_rows_out) == 0) {
  message("")
  message(">>> [ingest] nothing to write.")
} else if (isTRUE(DRY_RUN)) {
  message("")
  message(">>> [ingest] DRY RUN — no write performed. ", nrow(new_rows_out),
          " row(s) would be ",
          if (tab_exists) "appended to" else "written to a new tab named",
          " '", STATS_TAB, "'.")
  message(">>> [ingest] preview:")
  print(utils::head(as.data.frame(
    new_rows_out[, c("match_id", "team", "player_name", "player_id",
                     "minutes_played", "goals", "assists", "own_goals",
                     "result")]), 12), row.names = FALSE)
  message(">>> [ingest] Set DRY_RUN <- FALSE in the file to apply.")
} else {
  if (!tab_exists) {
    write_sheet(new_rows_out, ss = SHEET, sheet = STATS_TAB)
    message(">>> [ingest] created tab '", STATS_TAB, "' with ",
            nrow(new_rows_out), " rows")
  } else {
    sheet_append(SHEET, data = new_rows_out, sheet = STATS_TAB)
    message(">>> [ingest] appended ", nrow(new_rows_out), " rows")
  }
  
  # Hard verification: re-read fresh and confirm every ingested match is
  # present with the row count we sent.
  chk <- range_read(SHEET, sheet = STATS_TAB, col_types = "c")
  want <- table(new_rows_out$match_id)
  got  <- table(as.character(chk$match_id))
  bad  <- names(want)[is.na(got[names(want)]) |
                        got[names(want)] < want[names(want)]]
  if (length(bad) > 0) {
    stop("WRITE VERIFY FAILED — match(es) missing or short on the tab: ",
         paste(bad, collapse = ", "), call. = FALSE)
  }
  message(">>> [ingest] WRITE VERIFIED — tab now holds ", nrow(chk),
          " rows across ", length(unique(chk$match_id)), " match(es)")
  message(">>> [ingest] Next: run map_players_2627.R to close any mapping ",
          "gaps, then reset DRY_RUN <- TRUE in this file.")
}
