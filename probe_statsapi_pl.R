# Probe TheStatsAPI for Premier League player-stats coverage. (v1)
#
# READ-ONLY: makes GET requests to TheStatsAPI and writes raw JSON
# samples to data/api_samples_pl/. No Google Sheet reads or writes,
# no changes to any app data.
#
# Purpose: before building a PL contest on the WC26 infrastructure,
# confirm (a) the PL competition/season IDs, and (b) exactly which
# per-player stat fields the API collects for individual PL matches.
#
# Prerequisites:
#   - THESTATSAPI_KEY set in .Renviron
#   - httr2 installed
#
# Usage: source("probe_statsapi_pl.R")
# Safe to re-run any number of times; samples are overwritten.

library(httr2)
library(jsonlite)

message(">>> SOURCED probe_statsapi_pl.R (v1) at ", format(Sys.time()))

# --- Config -------------------------------------------------------------

STATS_API_BASE <- "https://api.thestatsapi.com/api"

SAMPLES_DIR <- "data/api_samples_pl"

stats_api_key <- function() {
  key <- Sys.getenv("THESTATSAPI_KEY")
  if (!nzchar(key)) {
    stop(
      "THESTATSAPI_KEY not set. Add it to .Renviron and restart the R session.",
      call. = FALSE
    )
  }
  key
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# --- Request helper (Rule 14: markers + outcome logs) -------------------

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

# Recursively collect every field name present anywhere in a parsed
# JSON structure — quick inventory of what an endpoint exposes.
collect_field_names <- function(x) {
  if (!is.list(x)) return(character(0))
  nm <- names(x) %||% character(0)
  unique(c(nm, unlist(lapply(x, collect_field_names))))
}

# --- Run ----------------------------------------------------------------

dir.create(SAMPLES_DIR, recursive = TRUE, showWarnings = FALSE)

# 1. Discover the Premier League competition ID --------------------------

comps <- api_get("football/competitions",
                 params = list(search = "premier league"),
                 save_as = "01_competitions_search")

comp_list <- comps$data %||% list()
if (length(comp_list) == 0) {
  stop(">>> [probe] No competitions matched 'premier league' — check ",
       SAMPLES_DIR, "/01_competitions_search.json", call. = FALSE)
}

# country may be a plain string or a nested object depending on endpoint.
country_of <- function(cm) {
  ct <- cm$country
  if (is.list(ct)) as.character(ct$name %||% "?") else as.character(ct %||% "?")
}

message(">>> [probe] competitions matching 'premier league':")
for (cm in comp_list) {
  message("    - ", cm$id %||% "?", "  ", cm$name %||% "?",
          "  [", country_of(cm), "]")
}

# Prefer the English one; fall back to first result. Hard-code the ID
# below on subsequent runs if the auto-pick chooses wrongly.
PL_COMPETITION_ID <- NULL
for (cm in comp_list) {
  if (grepl("england", tolower(country_of(cm)))) {
    PL_COMPETITION_ID <- cm$id
    break
  }
}
PL_COMPETITION_ID <- PL_COMPETITION_ID %||% comp_list[[1]]$id
message(">>> [probe] using competition: ", PL_COMPETITION_ID)

# 2. Discover the current season ----------------------------------------

seasons <- api_get(
  paste0("football/competitions/", PL_COMPETITION_ID, "/seasons"),
  save_as = "02_pl_seasons"
)

season_list <- seasons$data %||% list()
if (length(season_list) == 0) {
  stop(">>> [probe] No seasons returned — check ",
       SAMPLES_DIR, "/02_pl_seasons.json", call. = FALSE)
}

message(">>> [probe] seasons available:")
for (sn in season_list) {
  message("    - ", sn$id %||% "?", "  ", sn$name %||% sn$year %||% "?",
          if (isTRUE(sn$is_current) || isTRUE(sn$current)) "  (current)" else "")
}

PL_SEASON_ID <- NULL
for (sn in season_list) {
  if (isTRUE(sn$is_current) || isTRUE(sn$current)) { PL_SEASON_ID <- sn$id; break }
}
PL_SEASON_ID <- PL_SEASON_ID %||% season_list[[1]]$id
message(">>> [probe] using season: ", PL_SEASON_ID)

# 3. Find a finished match ------------------------------------------------
# One page of 100 is plenty for finding a finished fixture; 25/26 season
# just ended so anything recent works.

payload <- api_get("football/matches",
                   params = list(
                     competition_id = PL_COMPETITION_ID,
                     season_id      = PL_SEASON_ID,
                     per_page       = 100,
                     page           = 1
                   ),
                   save_as = "03_fixtures_page1")

fixtures <- payload$data %||% list()
statuses <- vapply(fixtures, function(f) as.character(f$status %||% NA),
                   character(1))
message(">>> [probe] fixtures loaded: ", length(fixtures),
        " — statuses: ",
        paste(names(table(statuses)), table(statuses),
              sep = "=", collapse = ", "))

pick <- NULL
hits <- fixtures[statuses == "finished"]
if (length(hits) > 0) pick <- hits[[length(hits)]]  # most recent finished on page

if (is.null(pick)) {
  message(">>> [probe] No finished PL matches on page 1 — the season/",
          "fixture samples are saved; inspect them and adjust paging.")
} else {
  match_id <- pick$id
  home <- pick$home_team$name %||% "?"
  away <- pick$away_team$name %||% "?"
  message(">>> [probe] sampling match ", match_id, ": ", home, " vs ", away,
          " (", pick$status %||% "?", ", kickoff ",
          pick$utc_date %||% "?", ")")
  
  # 4. Hit every per-match endpoint ---------------------------------------
  
  samples <- list()
  samples[["detail"]] <- api_get(
    paste0("football/matches/", match_id),
    save_as = "04_match_detail"
  )
  
  endpoints <- c("lineups", "timeline", "stats", "live-stats",
                 "player-stats", "shotmap")
  for (ep in endpoints) {
    samples[[ep]] <- api_get(
      paste0("football/matches/", match_id, "/", ep),
      save_as = paste0("04_match_", gsub("-", "_", ep))
    )
  }
  
  # 5. Per-player stat field inventory ------------------------------------
  # player-stats is the pipeline's food source, so break its per-player
  # stat keys out explicitly rather than burying them in the flat list.
  
  ps <- samples[["player-stats"]]
  if (!is.null(ps)) {
    players <- ps$data %||% ps$players %||% list()
    # The WC payload nests per-player stats; collect keys from every
    # player entry so sparse fields (e.g. GK-only) aren't missed.
    stat_keys <- character(0)
    for (p in players) {
      stat_keys <- union(stat_keys, collect_field_names(p))
    }
    message("")
    message(">>> [probe] ========= player-stats: PER-PLAYER FIELDS =========")
    message(">>> [probe] ", length(players), " player entries; fields:")
    for (k in sort(stat_keys)) message("    - ", k)
  } else {
    message(">>> [probe] player-stats returned nothing — see error JSON.")
  }
  
  # 6. Field inventory + scoring input check (all endpoints) --------------
  
  message("")
  message(">>> [probe] ================ FIELD INVENTORY ================")
  for (ep in names(samples)) {
    flds <- sort(collect_field_names(samples[[ep]]))
    message(">>> [probe] ", ep, " fields: ",
            if (length(flds) > 0) paste(flds, collapse = ", ") else "(none)")
  }
  
  all_fields <- tolower(unlist(lapply(samples, collect_field_names)))
  
  check <- function(label, patterns) {
    hit <- any(vapply(patterns,
                      function(p) any(grepl(p, all_fields)), logical(1)))
    message(">>> [probe] ", if (hit) "FOUND   " else "MISSING ", label)
  }
  
  message("")
  message(">>> [probe] ============== SCORING INPUT CHECK ==============")
  check("shots on target",     c("on_target", "ontarget", "sot",
                                 "shotsongoal"))
  check("saves (GK)",          c("save"))
  check("own goal marker",     c("own_goal", "owngoal", "goal_type"))
  check("penalty events",      c("penalty", "pen_", "is_penalty"))
  check("minutes played",      c("minute"))
  check("cards",               c("card"))
  check("substitution events", c("sub"))
  check("clean sheet inputs",  c("conceded", "goals_against"))
  check("assists",             c("assist"))
  message(">>> [probe] (MISSING here means not in these samples — check ",
          "the JSON files in ", SAMPLES_DIR, " before concluding)")
}

message("")
message(">>> [probe] Done. Raw samples in ", SAMPLES_DIR, "/")