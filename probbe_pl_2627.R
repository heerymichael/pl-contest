# Probe TheStatsAPI for the 26/27 Premier League season. (v1)
#
# READ-ONLY: makes GET requests to TheStatsAPI and writes raw JSON
# samples to data/api_samples_pl/. No Google Sheet reads or writes,
# no changes to any app data.
#
# Purpose: pre-season groundwork for the PL contest. Answers:
#   (a) does the API have a 26/27 season under comp_3039 yet, and
#       what is its season ID?
#   (b) are the 380 fixtures loaded, with matchday (= our gameweek)
#       populated?
#   (c) do fixtures carry kickoff datetimes — specifically the GW1
#       Arsenal v Coventry opener, which sets lock_at?
#   (d) the 20 clubs (falls out of the fixture list; feeds the teams
#       tab and squad-ingest targets)
#
# Prerequisites: THESTATSAPI_KEY in .Renviron; httr2 installed.
# Usage: source("probe_pl_2627.R")
# Safe to re-run any number of times; samples are overwritten.
# Budget: ~5 requests (1 seasons + 4 fixture pages), Sys.sleep(0.6).

library(httr2)
library(jsonlite)

message(">>> SOURCED probe_pl_2627.R (v1) at ", format(Sys.time()))

# --- Config -------------------------------------------------------------

STATS_API_BASE    <- "https://api.thestatsapi.com/api"
SAMPLES_DIR       <- "data/api_samples_pl"
PL_COMPETITION_ID <- "comp_3039"   # verified 5 Jul 2026

stats_api_key <- function() {
  key <- Sys.getenv("THESTATSAPI_KEY")
  if (!nzchar(key)) {
    stop("THESTATSAPI_KEY not set. Add it to .Renviron and restart R.",
         call. = FALSE)
  }
  key
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

api_get <- function(path, params = list(), save_as) {
  message(">>> [probe] GET ", path,
          if (length(params) > 0) {
            paste0("  [", paste(names(params), unlist(params),
                                sep = "=", collapse = ", "), "]")
          } else "")
  
  resp <- tryCatch(
    request(STATS_API_BASE) |>
      req_url_path_append(path) |>
      req_url_query(!!!params) |>
      req_auth_bearer_token(stats_api_key()) |>
      req_error(is_error = function(resp) FALSE) |>
      req_perform(),
    error = function(e) {
      message("    [probe] TRANSPORT FAILURE: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(resp)) return(NULL)
  
  status   <- resp_status(resp)
  raw_json <- tryCatch(resp_body_string(resp), error = function(e) "")
  
  if (status >= 400) {
    err <- tryCatch(fromJSON(raw_json, simplifyVector = FALSE)$error,
                    error = function(e) NULL)
    if (!is.null(err)) {
      message("    [probe] HTTP ", status,
              " — code: ",    err$code    %||% "?",
              " — message: ", err$message %||% "?")
    } else {
      message("    [probe] HTTP ", status, " — no parseable error body")
    }
    if (nzchar(raw_json)) {
      err_path <- file.path(SAMPLES_DIR, paste0(save_as, "_ERROR.json"))
      writeLines(raw_json, err_path)
      message("    [probe] error body saved ", err_path)
    }
    return(NULL)
  }
  
  out_path <- file.path(SAMPLES_DIR, paste0(save_as, ".json"))
  writeLines(prettify(raw_json), out_path)
  message("    [probe] OK (HTTP ", status, ") — saved ", out_path)
  
  fromJSON(raw_json, simplifyVector = FALSE)
}

# --- Run ----------------------------------------------------------------

dir.create(SAMPLES_DIR, recursive = TRUE, showWarnings = FALSE)

# 1. Seasons under comp_3039 — is 26/27 there? ---------------------------

seasons <- api_get(
  paste0("football/competitions/", PL_COMPETITION_ID, "/seasons"),
  save_as = "2627_01_seasons"
)

season_list <- seasons$data %||% list()
if (length(season_list) == 0) {
  stop(">>> [probe] No seasons returned — check ",
       SAMPLES_DIR, "/2627_01_seasons.json", call. = FALSE)
}

message(">>> [probe] seasons available:")
for (sn in season_list) {
  message("    - ", sn$id %||% "?", "  ", sn$name %||% sn$year %||% "?",
          if (isTRUE(sn$is_current) || isTRUE(sn$current)) "  (current)" else "")
}

# Look for a 26/27-shaped name; fall back to whatever is flagged current.
season_name <- function(sn) as.character(sn$name %||% sn$year %||% "")
PL_2627_ID <- NULL
for (sn in season_list) {
  if (grepl("26.?27|2026", season_name(sn))) { PL_2627_ID <- sn$id; break }
}
if (is.null(PL_2627_ID)) {
  for (sn in season_list) {
    if (isTRUE(sn$is_current) || isTRUE(sn$current)) {
      PL_2627_ID <- sn$id
      message(">>> [probe] no 26/27-named season; falling back to the ",
              "one flagged current: ", PL_2627_ID,
              " (", season_name(sn), ") — VERIFY this is really 26/27")
      break
    }
  }
}
if (is.null(PL_2627_ID)) {
  stop(">>> [probe] 26/27 season NOT FOUND under ", PL_COMPETITION_ID,
       " — the API hasn't loaded it yet. Re-run this probe weekly; ",
       "meanwhile the ingest adaptation proceeds against 25/26 data.",
       call. = FALSE)
}
message(">>> [probe] using season: ", PL_2627_ID)

# 2. Pull all fixture pages ---------------------------------------------

fixtures <- list()
for (pg in 1:5) {
  Sys.sleep(0.6)
  payload <- api_get("football/matches",
                     params = list(
                       competition_id = PL_COMPETITION_ID,
                       season_id      = PL_2627_ID,
                       per_page       = 100,
                       page           = pg
                     ),
                     save_as = sprintf("2627_02_fixtures_p%d", pg))
  page_fixtures <- payload$data %||% list()
  fixtures <- c(fixtures, page_fixtures)
  if (length(page_fixtures) < 100) break
}

message(">>> [probe] fixtures loaded: ", length(fixtures),
        " (expect 380 for a full season)")

if (length(fixtures) == 0) {
  stop(">>> [probe] Season exists but no fixtures loaded yet — ",
       "re-run weekly.", call. = FALSE)
}

# 3. Coverage summary -----------------------------------------------------

statuses  <- vapply(fixtures, function(f) as.character(f$status %||% NA),
                    character(1))
matchdays <- vapply(fixtures, function(f) {
  md <- f$matchday %||% f$round %||% NA
  suppressWarnings(as.numeric(md))
}, numeric(1))
kickoffs  <- vapply(fixtures, function(f)
  as.character(f$utc_date %||% f$date %||% NA), character(1))

message(">>> [probe] statuses: ",
        paste(names(table(statuses)), table(statuses),
              sep = "=", collapse = ", "))
message(">>> [probe] matchday populated: ", sum(!is.na(matchdays)),
        "/", length(fixtures),
        " — range ", suppressWarnings(min(matchdays, na.rm = TRUE)),
        "-", suppressWarnings(max(matchdays, na.rm = TRUE)))
message(">>> [probe] kickoff datetime populated: ",
        sum(!is.na(kickoffs) & nzchar(kickoffs)), "/", length(fixtures))

# 4. Club list -------------------------------------------------------------

club_of <- function(t) as.character(t$name %||% "?")
clubs <- sort(unique(c(
  vapply(fixtures, function(f) club_of(f$home_team), character(1)),
  vapply(fixtures, function(f) club_of(f$away_team), character(1))
)))
message(">>> [probe] clubs (", length(clubs), "):")
for (cl in clubs) message("    - ", cl)

# 5. GW1 fixtures + the opener --------------------------------------------

gw1 <- fixtures[!is.na(matchdays) & matchdays == 1]
message(">>> [probe] GW1 fixtures (", length(gw1), "):")
for (f in gw1) {
  message("    - ", f$id %||% "?", "  ",
          club_of(f$home_team), " v ", club_of(f$away_team),
          "  kickoff: ", f$utc_date %||% f$date %||% "?")
}

opener <- NULL
for (f in gw1) {
  if (grepl("arsenal", tolower(club_of(f$home_team))) &&
      grepl("coventry", tolower(club_of(f$away_team)))) { opener <- f; break }
}
if (!is.null(opener)) {
  message(">>> [probe] OPENER FOUND: ", opener$id,
          "  Arsenal v Coventry — kickoff ",
          opener$utc_date %||% opener$date %||% "NOT SET",
          "  -> lock_at = kickoff minus 1h (set in contest_config)")
} else {
  message(">>> [probe] Arsenal v Coventry not found in GW1 — check the ",
          "GW1 list above (fixture may differ from expectation)")
}

message("")
message(">>> [probe] Done. Raw samples in ", SAMPLES_DIR, "/")