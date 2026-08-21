# Probe TheStatsAPI for the 26/27 Premier League season. (v2)
#
# READ-ONLY: makes GET requests to TheStatsAPI and writes raw JSON
# samples to data/api_samples_pl/. No Google Sheet reads or writes,
# no changes to any app data.
#
# Purpose: pre-season groundwork for the PL contest. Answers:
#   (a) does the API have a 26/27 season under comp_3039 yet, and
#       what is its season ID?
#   (b) are the 380 fixtures loaded, with matchday populated?
#   (c) do UNPLAYED fixtures carry kickoff datetimes? This is the
#       fork for the gameweek validator's fixture cross-check and
#       for any fixture-list section in the app.
#   (d) the 20 clubs (feeds the teams tab and squad-ingest targets)
#
# v2 changes:
#   - season matcher no longer matches 2025/2026 on a bare "2026"
#   - no stop() on informative branches; ends with an explicit VERDICT
#   - auth/transport failure distinguished from "season not loaded"
#   - field inventory printed for one fixture object
#   - per-matchday fixture counts (should be 10 each)
#   - tidy CSV written for downstream gameweek validation
#
# Prerequisites: THESTATSAPI_KEY in .Renviron; httr2 installed.
# Usage: probe <- source("probe_pl_2627.R")$value
#    or: source("probe_pl_2627.R")   # verdict printed either way
# Safe to re-run any number of times; samples are overwritten.
# Budget: ~5 requests (1 seasons + up to 4 fixture pages), Sys.sleep(0.6).

library(httr2)
library(jsonlite)

message(">>> SOURCED probe_pl_2627.R (v2) at ", format(Sys.time()))

# --- Config -------------------------------------------------------------

STATS_API_BASE    <- "https://api.thestatsapi.com/api"
SAMPLES_DIR       <- "data/api_samples_pl"
PL_COMPETITION_ID <- "comp_3039"   # verified 5 Jul 2026

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

stats_api_key <- function() {
  key <- Sys.getenv("THESTATSAPI_KEY")
  if (!nzchar(key)) {
    stop("THESTATSAPI_KEY not set. Add it to .Renviron and restart R.",
         call. = FALSE)
  }
  key
}

# Returns list(ok, status, data). status is NA on transport failure.
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
  if (is.null(resp)) {
    return(list(ok = FALSE, status = NA_integer_, data = NULL))
  }
  
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
    return(list(ok = FALSE, status = status, data = NULL))
  }
  
  out_path <- file.path(SAMPLES_DIR, paste0(save_as, ".json"))
  writeLines(prettify(raw_json), out_path)
  message("    [probe] OK (HTTP ", status, ") — saved ", out_path)
  
  list(ok = TRUE, status = status,
       data = fromJSON(raw_json, simplifyVector = FALSE))
}

verdict <- function(code, detail) {
  message("")
  message(">>> [probe] ===== VERDICT: ", code, " =====")
  message(">>> [probe] ", detail)
  message("")
  invisible(list(verdict = code, detail = detail))
}

# --- Main ---------------------------------------------------------------

probe_main <- function() {
  
  dir.create(SAMPLES_DIR, recursive = TRUE, showWarnings = FALSE)
  
  # 1. Seasons under comp_3039 ------------------------------------------
  
  seasons_resp <- api_get(
    paste0("football/competitions/", PL_COMPETITION_ID, "/seasons"),
    save_as = "2627_01_seasons"
  )
  
  if (!isTRUE(seasons_resp$ok)) {
    st <- seasons_resp$status
    if (is.na(st)) {
      return(verdict("TRANSPORT_FAILURE",
                     "Could not reach the API at all. Nothing learned about 26/27."))
    }
    if (st %in% c(401L, 403L)) {
      return(verdict("AUTH_FAILURE",
                     paste0("HTTP ", st, " on the seasons endpoint. THESTATSAPI_KEY is ",
                            "missing, expired or lacks scope. This tells us NOTHING ",
                            "about whether 26/27 is loaded — fix auth and re-run.")))
    }
    return(verdict("SEASONS_ENDPOINT_FAILED",
                   paste0("HTTP ", st, " on the seasons endpoint. See ",
                          SAMPLES_DIR, "/2627_01_seasons_ERROR.json")))
  }
  
  season_list <- seasons_resp$data$data %||% list()
  if (length(season_list) == 0) {
    return(verdict("NO_SEASONS_RETURNED",
                   paste0("Endpoint returned 200 but an empty season list. Inspect ",
                          SAMPLES_DIR, "/2627_01_seasons.json — the payload shape may ",
                          "have changed (data may not be under $data).")))
  }
  
  season_name <- function(sn) as.character(sn$name %||% sn$year %||% "")
  
  message(">>> [probe] seasons available (", length(season_list), "):")
  for (sn in season_list) {
    message("    - ", sn$id %||% "?", "  ", season_name(sn),
            if (isTRUE(sn$is_current) || isTRUE(sn$current)) "  (current)" else "")
  }
  
  # Tightened matcher. A 26/27 season starts in 2026 and ends in 2027, so
  # require BOTH years, or an explicit 26/27-style pair. Critically, reject
  # anything mentioning 2025 or a 25/26 pair — "2025/2026" contains "2026"
  # and matched the v1 pattern, which would have silently selected 25/26.
  looks_2627 <- function(nm) {
    nm <- gsub("\\s", "", nm)
    if (grepl("2025|(^|[^0-9])25[/\\-]26([^0-9]|$)", nm)) return(FALSE)
    grepl("2026.*2027", nm) ||
      grepl("(^|[^0-9])26[/\\-]27([^0-9]|$)", nm) ||
      grepl("(^|[^0-9])2026[/\\-]27([^0-9]|$)", nm) ||
      identical(nm, "2026")
  }
  
  PL_2627_ID <- NULL
  for (sn in season_list) {
    if (looks_2627(season_name(sn))) {
      PL_2627_ID <- sn$id
      message(">>> [probe] matched 26/27 season by name: ",
              PL_2627_ID, " (", season_name(sn), ")")
      break
    }
  }
  
  if (is.null(PL_2627_ID)) {
    cur <- NULL
    for (sn in season_list) {
      if (isTRUE(sn$is_current) || isTRUE(sn$current)) { cur <- sn; break }
    }
    detail <- paste0(
      "No season under ", PL_COMPETITION_ID, " matches a 26/27 naming ",
      "pattern. The API has not loaded it yet.")
    if (!is.null(cur)) {
      detail <- paste0(detail, " Season flagged current is ",
                       cur$id, " (", season_name(cur), ") — NOT used, ",
                       "since selecting it would produce a false positive.")
    }
    detail <- paste0(detail, " Re-run this probe weekly. Meanwhile the ",
                     "gameweek fixture cross-check is written dormant and ",
                     "the app fixture list needs another source.")
    return(verdict("SEASON_ABSENT", detail))
  }
  
  message(">>> [probe] using season: ", PL_2627_ID)
  
  # 2. Fixture pages -----------------------------------------------------
  
  fixtures <- list()
  for (pg in 1:5) {
    Sys.sleep(0.6)
    resp <- api_get("football/matches",
                    params = list(
                      competition_id = PL_COMPETITION_ID,
                      season_id      = PL_2627_ID,
                      per_page       = 100,
                      page           = pg
                    ),
                    save_as = sprintf("2627_02_fixtures_p%d", pg))
    if (!isTRUE(resp$ok)) {
      return(verdict("FIXTURES_ENDPOINT_FAILED",
                     paste0("Season ", PL_2627_ID, " exists but the matches endpoint ",
                            "failed on page ", pg, " (HTTP ", resp$status %||% "NA", ").")))
    }
    page_fixtures <- resp$data$data %||% list()
    fixtures <- c(fixtures, page_fixtures)
    if (length(page_fixtures) < 100) break
  }
  
  message(">>> [probe] fixtures loaded: ", length(fixtures),
          " (expect 380 for a full season)")
  
  if (length(fixtures) == 0) {
    return(verdict("SEASON_EMPTY",
                   paste0("Season ", PL_2627_ID, " exists but carries no fixtures yet. ",
                          "This is the expected intermediate state — re-run weekly.")))
  }
  
  # 3. Field inventory ---------------------------------------------------
  
  message(">>> [probe] field inventory, fixtures[[1]]:")
  message("    top-level: ", paste(names(fixtures[[1]]), collapse = ", "))
  for (nm in names(fixtures[[1]])) {
    v <- fixtures[[1]][[nm]]
    if (is.list(v) && length(v) > 0 && !is.null(names(v))) {
      message("    ", nm, " -> ", paste(names(v), collapse = ", "))
    }
  }
  
  # 4. Coverage ----------------------------------------------------------
  
  chr_of <- function(f, ...) {
    for (nm in c(...)) {
      v <- f[[nm]]
      if (!is.null(v) && length(v) > 0) return(as.character(v)[1])
    }
    NA_character_
  }
  
  ids       <- vapply(fixtures, function(f) chr_of(f, "id", "match_id"),
                      character(1))
  statuses  <- vapply(fixtures, function(f) chr_of(f, "status", "state"),
                      character(1))
  kickoffs  <- vapply(fixtures, function(f)
    chr_of(f, "utc_date", "date", "kickoff",
           "start_time", "scheduled_at"), character(1))
  matchdays <- suppressWarnings(as.numeric(
    vapply(fixtures, function(f) chr_of(f, "matchday", "round", "round_number"),
           character(1))))
  
  club_of <- function(t) as.character(t$name %||% t$short_name %||% "?")[1]
  home <- vapply(fixtures, function(f) club_of(f$home_team), character(1))
  away <- vapply(fixtures, function(f) club_of(f$away_team), character(1))
  
  message(">>> [probe] statuses: ",
          paste(names(table(statuses, useNA = "ifany")),
                table(statuses, useNA = "ifany"),
                sep = "=", collapse = ", "))
  
  n_md <- sum(!is.na(matchdays))
  message(">>> [probe] matchday populated: ", n_md, "/", length(fixtures),
          if (n_md > 0) paste0(" — range ", min(matchdays, na.rm = TRUE),
                               "-", max(matchdays, na.rm = TRUE)) else "")
  
  if (n_md > 0) {
    md_tab <- table(matchdays)
    odd <- names(md_tab)[md_tab != 10]
    message(">>> [probe] fixtures per matchday: ",
            if (length(odd) == 0) "all 10 (as expected)"
            else paste0("NOT 10 for matchday(s) ", paste(odd, collapse = ", "),
                        " — inspect before trusting round numbers"))
  }
  
  ko_parsed <- suppressWarnings(as.POSIXct(
    sub("Z$", "", kickoffs), format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"))
  if (all(is.na(ko_parsed))) {
    ko_parsed <- suppressWarnings(as.POSIXct(kickoffs, tz = "UTC"))
  }
  n_ko     <- sum(!is.na(ko_parsed))
  n_future <- sum(!is.na(ko_parsed) & ko_parsed > Sys.time())
  
  message(">>> [probe] kickoff parsed: ", n_ko, "/", length(fixtures),
          " — of which in the future: ", n_future)
  
  # 5. Clubs -------------------------------------------------------------
  
  clubs <- sort(unique(c(home, away)))
  message(">>> [probe] clubs (", length(clubs), "):")
  for (cl in clubs) message("    - ", cl)
  
  # 6. Tidy CSV for downstream gameweek validation -----------------------
  
  tidy <- data.frame(
    match_id   = ids,
    matchday   = matchdays,
    kickoff_utc = format(ko_parsed, "%Y-%m-%d %H:%M:%S"),
    home       = home,
    away       = away,
    status     = statuses,
    stringsAsFactors = FALSE
  )
  tidy <- tidy[order(tidy$kickoff_utc, tidy$match_id), ]
  csv_path <- file.path(SAMPLES_DIR, "2627_fixtures_tidy.csv")
  write.csv(tidy, csv_path, row.names = FALSE, na = "")
  message(">>> [probe] tidy fixture list written: ", csv_path)
  
  # 7. GW1 sample --------------------------------------------------------
  
  if (n_md > 0) {
    gw1 <- which(!is.na(matchdays) & matchdays == 1)
    message(">>> [probe] matchday 1 fixtures (", length(gw1), "):")
    for (i in gw1) {
      message("    - ", ids[i], "  ", home[i], " v ", away[i],
              "  kickoff(UTC): ", kickoffs[i] %||% "?")
    }
  }
  
  # 8. Verdict -----------------------------------------------------------
  
  if (n_future > 0) {
    return(verdict("FIXTURES_SCHEDULED",
                   paste0(n_future, " unplayed fixtures carry parseable kickoff times. ",
                          "The gameweek fixture cross-check can be built live, and an ",
                          "app fixture-list section is viable off this endpoint.")))
  }
  
  if (n_ko > 0) {
    return(verdict("FIXTURES_COMPLETED_ONLY",
                   paste0(n_ko, " fixtures carry kickoff times but none are in the ",
                          "future. The API exposes results, not a forward schedule. ",
                          "Cross-check gets written dormant; the app fixture list needs ",
                          "the PL site or a one-off hand entry.")))
  }
  
  verdict("FIXTURES_NO_KICKOFFS",
          paste0(length(fixtures), " fixtures returned but no parseable kickoff ",
                 "field. Check the field inventory above — the datetime key is ",
                 "not among the names tried."))
}

probe <- probe_main()
message(">>> [probe] Done. Raw samples in ", SAMPLES_DIR, "/")