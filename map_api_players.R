# map_api_players.R -----------------------------------------------------------
# Build / extend the mapping from TheStatsAPI player IDs to contest player_ids.
#
# Incremental by design: each run fetches player-stats for every FINISHED
# match, collects all API players seen (including unused subs), matches any
# not-yet-mapped ones against the Sheet's players tab, and writes the result
# to column H (api_player_id) of the players tab.
#
# Safety properties:
#   - NEVER overwrites an existing non-empty api_player_id cell (manual
#     fixes in the Sheet are preserved)
#   - Ambiguous or failed matches are NOT guessed — they go to the report
#     file for manual resolution: data/api_player_mapping_report.csv
#   - Per-match player-stats are cached to data/api_cache/ (immutable once a
#     match is final) — re-runs only hit the API for newly finished matches.
#     The fixtures list is NOT cached (see v4).
#
# v2: candidate pool filtered by team SHORT CODE; DR Congo / Bosnia aliases.
# v3: portable accent normalisation (macOS iconv TRANSLIT mangles accents
#     into apostrophe sequences); position used as a fuzzy TIEBREAK rather
#     than a hard filter (API positions are blank ~20% and sometimes wrong);
#     token-subset rule for middle-name variants.
# v4: fixtures list fetched LIVE every run (api_call use_cache = FALSE). It was
#     previously disk-cached and never invalidated, so re-runs re-read a frozen
#     snapshot and never saw newly finished matches / squads.
#
# Run with: source("map_api_players.R")
# ------------------------------------------------------------------------------

message(">>> SOURCED map_api_players.R (v4) at ",
        format(Sys.time(), "%Y-%m-%d %H:%M:%S"))

library(httr)
library(jsonlite)
library(dplyr)

source("R/sheets.R")   # gs_auth(), read_sheet_tab(), sheet_id()

`%||%` <- function(a, b) if (is.null(a)) b else a

# --- Config -------------------------------------------------------------------

BASE_URL    <- "https://api.thestatsapi.com/api/football"
COMP_ID     <- "comp_6107"
SEASON_ID   <- "sn_118868"
CALL_SLEEP  <- 0.6
CACHE_DIR   <- "data/api_cache"
REPORT_FILE <- "data/api_player_mapping_report.csv"

# API naming <-> Sheet naming variants (normalised forms, see norm_name below)
TEAM_ALIASES <- c(
  "czech republic"     = "czechia",
  "korea republic"     = "south korea",
  "usa"                = "united states",
  "ir iran"            = "iran",
  "cote divoire"       = "ivory coast",
  "dr congo"           = "congo dr",
  "bosnia herzegovina" = "bosnia and herzegovina"
)

dir.create(CACHE_DIR, recursive = TRUE, showWarnings = FALSE)

get_api_key <- function() {
  key <- Sys.getenv("THESTATSAPI_KEY")
  if (nzchar(key)) return(key)
  keyfile <- path.expand("~/.thestatsapi_key")
  if (file.exists(keyfile)) return(trimws(readLines(keyfile, n = 1L)))
  stop("No API key: set THESTATSAPI_KEY or create ~/.thestatsapi_key")
}
API_KEY <- get_api_key()

# --- Cached API wrapper (per field notes) --------------------------------------

cache_path <- function(endpoint, query) {
  qs <- if (length(query)) {
    paste0("__", paste(names(query), unlist(query), sep = "-", collapse = "_"))
  } else ""
  fname <- paste0(gsub("[^a-zA-Z0-9]+", "_", endpoint), qs, ".json")
  file.path(CACHE_DIR, fname)
}

# use_cache = FALSE bypasses the disk cache entirely (no read, no write) — used
# for the fixtures list, which by definition changes over the tournament. Per-
# match player-stats are immutable once finished and keep use_cache = TRUE.
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

# --- Name / position normalisation --------------------------------------------

# Portable accent stripping: explicit character map instead of iconv TRANSLIT,
# which on macOS produces apostrophe sequences ("í" -> "'i") that break exact
# matching whenever only one side of the comparison carries the accent.
ACCENT_FROM <- "àáâãäåāăąçćčďđèéêëēėęěìíîïīįñńňòóôõöøōőřśšşťùúûüūůűýÿžźżğľĺŕșţ"
ACCENT_TO   <- "aaaaaaaaacccddeeeeeeeeiiiiiinnnoooooooorssstuuuuuuuyyzzzgllrst"

norm_name <- function(x) {
  x <- tolower(x)
  x <- gsub("ß", "ss", x, fixed = TRUE)
  x <- chartr(ACCENT_FROM, ACCENT_TO, x)
  x <- gsub("[^a-z ]", " ", x)
  trimws(gsub(" +", " ", x))
}

token_sort <- function(x) {
  vapply(strsplit(x, " "), function(t) paste(sort(t), collapse = " "), character(1))
}

# API position letter -> Sheet position
API_POS <- c(G = "GK", D = "DEF", M = "MID", F = "FWD")

pos_compatible <- function(api_pos, sheet_pos) {
  if (is.na(api_pos) || !nzchar(api_pos)) return(TRUE)   # blank API pos: no veto
  mapped <- API_POS[[api_pos]] %||% NA_character_
  is.na(mapped) || identical(mapped, sheet_pos)
}

# --- Load Sheet players --------------------------------------------------------

message(">>> [sheets] authenticating and reading players tab")
gs_auth()
players <- read_sheet_tab("players")
message(">>> [sheets] players tab: ", nrow(players), " rows, columns: ",
        paste(names(players), collapse = ", "))

if (!"api_player_id" %in% names(players)) {
  players$api_player_id <- NA_character_
  message(">>> [sheets] api_player_id column not present yet — will create as column H")
} else {
  players$api_player_id <- as.character(players$api_player_id)
}

players <- players %>%
  mutate(
    .row       = row_number(),
    name_norm  = norm_name(name),
    name_sort  = token_sort(name_norm)
  )

# --- Build API team -> contest team mapping from fixtures ----------------------

message(">>> [api] fetching fixtures (all pages)")
fixtures <- list()
page <- 1L
repeat {
  # Fixtures fetched LIVE every run (use_cache = FALSE) — status/scores change,
  # and a cached snapshot would freeze which matches look finished, so new
  # squads would never be seen for mapping.
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
message(">>> [api] fixtures: ", length(fixtures), " matches")

api_teams <- unique(do.call(rbind, lapply(fixtures, function(m) {
  rbind(
    data.frame(api_team_id = m$home_team$id %||% NA_character_,
               api_team_name = m$home_team$name %||% NA_character_,
               stringsAsFactors = FALSE),
    data.frame(api_team_id = m$away_team$id %||% NA_character_,
               api_team_name = m$away_team$name %||% NA_character_,
               stringsAsFactors = FALSE)
  )
})))

sheet_teams <- read_sheet_tab("teams") %>%
  mutate(team_name_norm = norm_name(name))

api_teams$api_name_norm <- norm_name(api_teams$api_team_name)

# Apply aliases in BOTH directions (api variant -> sheet variant and reverse)
alias_lookup <- c(TEAM_ALIASES, setNames(names(TEAM_ALIASES), TEAM_ALIASES))

api_teams$match_norm <- ifelse(
  api_teams$api_name_norm %in% sheet_teams$team_name_norm,
  api_teams$api_name_norm,
  alias_lookup[api_teams$api_name_norm]
)

team_map <- api_teams %>%
  left_join(sheet_teams, by = c("match_norm" = "team_name_norm"))

unmatched_teams <- team_map %>% filter(is.na(team_id))
if (nrow(unmatched_teams) > 0) {
  message(">>> [teams] WARNING — unmatched API teams (add to TEAM_ALIASES and re-run):")
  for (i in seq_len(nrow(unmatched_teams))) {
    message("    ", unmatched_teams$api_team_name[i],
            " (", unmatched_teams$api_team_id[i], ")")
  }
}
message(">>> [teams] mapped ", sum(!is.na(team_map$team_id)), " of ",
        nrow(team_map), " API teams seen in fixtures")

# --- Collect API players from all finished matches -----------------------------

finished <- Filter(function(m) identical(m$status %||% "", "finished"), fixtures)
message(">>> [api] finished matches: ", length(finished))

api_players <- list()
for (m in finished) {
  ps <- api_call(paste0("/matches/", m$id, "/player-stats"))
  if (is.null(ps)) next
  for (p in (ps$data %||% list())) {
    pid <- p$player_id %||% NA_character_
    if (is.na(pid)) next
    if (is.null(api_players[[pid]])) {
      api_players[[pid]] <- data.frame(
        api_player_id = pid,
        api_name      = p$player_name %||% NA_character_,
        api_team_id   = p$team_id     %||% NA_character_,
        api_pos       = p$position    %||% "",
        stringsAsFactors = FALSE
      )
    } else if (!nzchar(api_players[[pid]]$api_pos) && nzchar(p$position %||% "")) {
      api_players[[pid]]$api_pos <- p$position   # backfill blank position
    }
  }
}
api_df <- do.call(rbind, api_players)
message(">>> [api] unique API players seen so far: ",
        if (is.null(api_df)) 0 else nrow(api_df))
if (is.null(api_df)) stop("No API players collected — nothing to do")

# Bring across BOTH the display name and the short_code; the players tab's
# `team` column stores short codes (MEX/KOR/CZE), so matching uses short_code.
api_df <- api_df %>%
  left_join(team_map %>% select(api_team_id, team_id,
                                team_name = name, short_code),
            by = "api_team_id") %>%
  mutate(api_name_norm = norm_name(api_name),
         api_name_sort = token_sort(api_name_norm))

already_mapped_ids <- players$api_player_id[!is.na(players$api_player_id) &
                                              nzchar(players$api_player_id)]
todo <- api_df %>% filter(!(api_player_id %in% already_mapped_ids))
message(">>> [map] already mapped: ", nrow(api_df) - nrow(todo),
        "; to attempt this run: ", nrow(todo))

# --- Matching -------------------------------------------------------------------

match_one <- function(ap, pool) {
  cand <- pool %>% filter(is.na(api_player_id) | !nzchar(api_player_id))
  if (nrow(cand) == 0) return(list(row = NA_integer_, method = "no_candidates"))
  
  # 1. exact normalised name
  hit <- cand %>% filter(name_norm == ap$api_name_norm)
  if (nrow(hit) == 1) return(list(row = hit$.row, method = "exact"))
  
  # 2. token-sorted name (handles "Jae-sung Lee" vs "Lee Jae-sung")
  hit <- cand %>% filter(name_sort == ap$api_name_sort)
  if (nrow(hit) == 1) return(list(row = hit$.row, method = "token_sort"))
  
  # 3. token subset (middle-name variants: "Tholo Thabang Matuludi" vs
  #    "Thabang Matuludi"); smaller side must have >= 2 tokens, unique hit
  ap_tok <- strsplit(ap$api_name_norm, " ")[[1]]
  subset_hit <- integer(0)
  for (j in seq_len(nrow(cand))) {
    ct <- strsplit(cand$name_norm[j], " ")[[1]]
    small <- if (length(ap_tok) <= length(ct)) ap_tok else ct
    big   <- if (length(ap_tok) <= length(ct)) ct else ap_tok
    if (length(small) >= 2 && all(small %in% big)) {
      subset_hit <- c(subset_hit, j)
    }
  }
  if (length(subset_hit) == 1) {
    return(list(row = cand$.row[subset_hit], method = "token_subset"))
  }
  
  # 4. fuzzy across ALL unmapped same-team candidates; position used only to
  #    break near-ties, never to exclude (API positions are blank ~20% and
  #    occasionally wrong)
  d <- as.numeric(adist(ap$api_name_norm, cand$name_norm)) /
    pmax(nchar(ap$api_name_norm), nchar(cand$name_norm))
  ord <- order(d)
  best <- d[ord[1]]
  if (best > 0.25) return(list(row = NA_integer_, method = "unmatched"))
  
  second <- if (length(d) > 1) d[ord[2]] else Inf
  if ((second - best) >= 0.10) {
    return(list(row = cand$.row[ord[1]], method = sprintf("fuzzy_%.2f", best)))
  }
  
  # near-tie: accept only if exactly one of the tied candidates is
  # position-compatible with the API's stated position
  tied <- ord[d[ord] - best < 0.10 & d[ord] <= 0.25]
  ok <- tied[vapply(tied, function(j)
    pos_compatible(ap$api_pos, cand$position[j]), logical(1))]
  if (length(ok) == 1) {
    return(list(row = cand$.row[ok], method = sprintf("fuzzy_pos_tiebreak_%.2f", d[ok])))
  }
  list(row = NA_integer_, method = "unmatched")
}

results <- data.frame(api_player_id = character(0), api_name = character(0),
                      team_name = character(0), api_pos = character(0),
                      method = character(0), matched_name = character(0),
                      stringsAsFactors = FALSE)
n_new <- 0L

for (i in seq_len(nrow(todo))) {
  ap <- todo[i, ]
  if (is.na(ap$team_id)) {
    results <- rbind(results, data.frame(
      api_player_id = ap$api_player_id, api_name = ap$api_name,
      team_name = ap$api_team_id, api_pos = ap$api_pos,
      method = "team_unmapped", matched_name = NA_character_,
      stringsAsFactors = FALSE))
    next
  }
  pool <- players %>% filter(team == ap$short_code)
  res  <- match_one(ap, pool)
  matched_name <- if (!is.na(res$row)) players$name[players$.row == res$row] else NA_character_
  
  if (!is.na(res$row)) {
    players$api_player_id[players$.row == res$row] <- ap$api_player_id
    n_new <- n_new + 1L
  }
  results <- rbind(results, data.frame(
    api_player_id = ap$api_player_id, api_name = ap$api_name,
    team_name = ap$team_name, api_pos = ap$api_pos,
    method = res$method, matched_name = matched_name,
    stringsAsFactors = FALSE))
}

message(">>> [map] outcome by method:")
print(table(results$method))

message(">>> [map] matched pairs for eyeball check (non-exact methods):")
review <- results %>% filter(!is.na(matched_name), method != "exact")
if (nrow(review) > 0) {
  print(review %>% select(api_name, matched_name, team_name, method),
        row.names = FALSE)
}

unmatched <- results %>% filter(is.na(matched_name))
if (nrow(unmatched) > 0) {
  write.csv(results, REPORT_FILE, row.names = FALSE)
  message(">>> [map] ", nrow(unmatched), " unmatched — full report: ", REPORT_FILE)
  message(">>> [map] resolve manually by pasting the api_player_id into column H ",
          "of the players tab")
  print(unmatched %>% select(api_player_id, api_name, team_name, api_pos),
        row.names = FALSE)
} else {
  write.csv(results, REPORT_FILE, row.names = FALSE)
  message(">>> [map] all matched — report written for audit: ", REPORT_FILE)
}

# --- Write column H back to the Sheet -------------------------------------------

if (n_new > 0) {
  message(">>> [sheets] writing api_player_id column (", n_new,
          " new mappings) to players tab column H")
  col_df <- data.frame(api_player_id = players$api_player_id)
  range_write(
    sheet_id(),
    data  = col_df,
    sheet = "players",
    range = paste0("H1:H", nrow(players) + 1L),
    col_names = TRUE,
    reformat  = FALSE
  )
  message(">>> [sheets] write complete")
  message(">>> NOTE: re-run snapshot_reference_data.R if the app should see ",
          "this column (ingestion reads the Sheet fresh, so not required ",
          "for scoring)")
} else {
  message(">>> [sheets] no new mappings — Sheet untouched")
}

message(">>> [map] done at ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))