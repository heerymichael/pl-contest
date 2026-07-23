# watch_player_stats.R ------------------------------------------------------
# Purpose: measure TheStatsAPI's post-match publication lag for World Cup 2026.
#
# Polls two endpoints for each opening-day match every 15 minutes:
#   - GET /matches/{id}            (canonical score + status)
#   - GET /matches/{id}/player-stats  (the endpoint our scoring depends on)
#
# Logs one timestamped line per match per cycle to console AND to
# data/api_samples/watch_log.txt. Stops automatically once player-stats has
# populated for BOTH matches, or after MAX_HOURS.
#
# On first success per match, saves the raw player-stats JSON to
# data/api_samples/ for scoring-input validation.
#
# Run with:  source("watch_player_stats.R")
# Stop early: Esc / Ctrl-C (log file survives).
# ---------------------------------------------------------------------------

message(">>> SOURCED watch_player_stats.R (v1) at ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))

library(httr)
library(jsonlite)

`%||%` <- function(a, b) if (is.null(a)) b else a

# --- Config ----------------------------------------------------------------

BASE_URL      <- "https://api.thestatsapi.com/api/football"
COMP_ID       <- "comp_6107"
SEASON_ID     <- "sn_118868"
POLL_MINUTES  <- 15
MAX_HOURS     <- 14          # give up after this long (covers overnight)
CALL_SLEEP    <- 0.6         # paid-plan spacing between consecutive calls
SAMPLES_DIR   <- "data/api_samples"
LOG_FILE      <- file.path(SAMPLES_DIR, "watch_log.txt")

dir.create(SAMPLES_DIR, recursive = TRUE, showWarnings = FALSE)

# --- API key (env var first, keyfile fallback) -----------------------------

get_api_key <- function() {
  key <- Sys.getenv("THESTATSAPI_KEY")
  if (nzchar(key)) return(key)
  keyfile <- path.expand("~/.thestatsapi_key")
  if (file.exists(keyfile)) return(trimws(readLines(keyfile, n = 1L)))
  stop("No API key: set THESTATSAPI_KEY in .Renviron or create ~/.thestatsapi_key")
}

API_KEY <- get_api_key()

# --- Logging ----------------------------------------------------------------

log_line <- function(...) {
  line <- paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", ...)
  message(">>> [watch] ", line)
  cat(line, "\n", file = LOG_FILE, append = TRUE)
}

# --- Minimal request wrapper (no caching: every poll must be a fresh read) --

api_get <- function(endpoint, query = list()) {
  url  <- paste0(BASE_URL, endpoint)
  resp <- tryCatch(
    GET(url, query = query,
        add_headers(Authorization = paste("Bearer", API_KEY)),
        timeout(30)),
    error = function(e) e
  )
  Sys.sleep(CALL_SLEEP)
  
  if (inherits(resp, "error")) {
    return(list(status = NA_integer_, body = NULL,
                error = conditionMessage(resp)))
  }
  
  status <- status_code(resp)
  
  if (status == 429L) {
    log_line("HTTP 429 on ", endpoint, " — sleeping 60s, one retry")
    Sys.sleep(60)
    resp <- GET(url, query = query,
                add_headers(Authorization = paste("Bearer", API_KEY)),
                timeout(30))
    Sys.sleep(CALL_SLEEP)
    status <- status_code(resp)
  }
  
  raw  <- content(resp, "text", encoding = "UTF-8")
  body <- tryCatch(fromJSON(raw, simplifyVector = FALSE),
                   error = function(e) NULL)
  list(status = status, body = body, raw = raw, error = NULL)
}

# --- Discover the two opening-day matches ----------------------------------

message(">>> [watch] fetching fixtures to identify target matches")
fx_resp <- api_get("/matches", query = list(
  competition_id = COMP_ID,
  season_id      = SEASON_ID,
  per_page       = 100,
  page           = 1
))
if (fx_resp$status != 200L) {
  stop("Fixture fetch failed (HTTP ", fx_resp$status, ") — aborting. ",
       fx_resp$error %||% "")
}

fixtures <- fx_resp$body$data

match_label <- function(m) {
  paste0(m$home_team$name %||% "?", " vs ", m$away_team$name %||% "?")
}

find_match <- function(home_pattern, away_pattern) {
  for (m in fixtures) {
    h <- m$home_team$name %||% ""
    a <- m$away_team$name %||% ""
    if (grepl(home_pattern, h, ignore.case = TRUE) &&
        grepl(away_pattern, a, ignore.case = TRUE)) {
      return(m)
    }
  }
  NULL
}

m1 <- find_match("Mexico", "South Africa")
m2 <- find_match("Korea",  "Czech")        # matches "Czechia" / "Czech Republic"

targets <- Filter(Negate(is.null), list(m1, m2))
if (length(targets) == 0L) stop("Could not identify either target match in fixtures")
if (is.null(m1)) log_line("WARNING: Mexico vs South Africa not found in fixtures")
if (is.null(m2)) log_line("WARNING: South Korea vs Czechia not found in fixtures")

for (m in targets) {
  log_line("watching ", m$id, ": ", match_label(m),
           " (kickoff ", m$utc_date %||% "?", ")")
}

# --- One poll cycle for one match -------------------------------------------

slug <- function(m) gsub("[^a-z0-9]+", "_", tolower(match_label(m)))

poll_match <- function(m, done_env) {
  id <- m$id
  
  # 1. Canonical score / status
  det <- api_get(paste0("/matches/", id))
  if (det$status == 200L) {
    d      <- det$body$data %||% det$body
    sc     <- d$score %||% list()
    score  <- paste0(sc$home %||% "?", "-", sc$away %||% "?")
    status <- d$status %||% "?"
  } else {
    score  <- "?"
    status <- paste0("detail HTTP ", det$status)
  }
  
  # 2. player-stats — the endpoint that decides everything
  ps <- api_get(paste0("/matches/", id, "/player-stats"))
  
  if (ps$status == 200L) {
    rows <- length(ps$body$data %||% list())
    if (rows > 0L) {
      log_line(id, " ", match_label(m), ": status=", status,
               " score=", score, " player-stats=200 (", rows,
               " rows) *** PUBLISHED ***")
      out <- file.path(SAMPLES_DIR,
                       paste0("player_stats_", slug(m), ".json"))
      writeLines(ps$raw, out)
      log_line("saved payload -> ", out)
      assign(id, TRUE, envir = done_env)
    } else {
      log_line(id, " ", match_label(m), ": status=", status,
               " score=", score, " player-stats=200 but EMPTY data array")
    }
  } else {
    log_line(id, " ", match_label(m), ": status=", status,
             " score=", score, " player-stats=HTTP ", ps$status,
             if (!is.null(ps$error)) paste0(" (", ps$error, ")") else "")
  }
  
  invisible(NULL)
}

# --- Main loop ---------------------------------------------------------------

done_env <- new.env()
deadline <- Sys.time() + MAX_HOURS * 3600
log_line("polling every ", POLL_MINUTES, " min until both matches publish ",
         "or ", format(deadline, "%Y-%m-%d %H:%M:%S"))

repeat {
  for (m in targets) {
    if (!isTRUE(get0(m$id, envir = done_env, ifnotfound = FALSE))) {
      poll_match(m, done_env)
    }
  }
  
  all_done <- all(vapply(
    targets,
    function(m) isTRUE(get0(m$id, envir = done_env, ifnotfound = FALSE)),
    logical(1)
  ))
  
  if (all_done) {
    log_line("all watched matches have published player-stats — done")
    break
  }
  if (Sys.time() >= deadline) {
    log_line("MAX_HOURS (", MAX_HOURS, ") reached without full publication — ",
             "stopping. This is strong evidence for the support email.")
    break
  }
  
  Sys.sleep(POLL_MINUTES * 60)
}

message(">>> [watch] finished at ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " — full history in ", LOG_FILE)