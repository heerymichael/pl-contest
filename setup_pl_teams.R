# setup_pl_teams.R -------------------------------------------------------
# One-time write of the `teams` tab to pl-contest-data: the 20 clubs of
# the 26/27 Premier League.
#
# Columns:
#   team_id    — contest-internal integer key (stable, arbitrary order)
#   name       — display name used throughout the app UI
#   short_code — 3-letter code; also keys the badge asset at
#                www/logos/<short_code>.svg (all 20 verified present)
#   api_name   — the club name as TheStatsAPI spells it, for joining
#                fixture/squad payloads to this tab. Verified against
#                the 25/26 fixture payloads for the 17 continuing
#                clubs; the 3 promoted clubs (COV, HUL, IPS) carry the
#                expected name and MUST be re-verified when the 26/27
#                season loads in the API (re-run probe_pl_2627.R).
#
# Safe to re-run only if the tab doesn't exist — refuses to overwrite.
#
# Run in two steps per the auth pattern:
#   source("R/sheets.R")
#   source("setup_pl_teams.R")

library(googlesheets4)
library(tibble)

message(">>> setup_pl_teams — target Sheet: ", sheet_id())

teams <- tribble(
  ~team_id, ~name,                     ~short_code, ~api_name,
  1L,      "Arsenal",                 "ARS",       "Arsenal",
  2L,      "Aston Villa",             "AVL",       "Aston Villa",
  3L,      "Bournemouth",             "BOU",       "Bournemouth",
  4L,      "Brentford",               "BRE",       "Brentford",
  5L,      "Brighton",                "BHA",       "Brighton & Hove Albion",
  6L,      "Chelsea",                 "CHE",       "Chelsea",
  7L,      "Coventry City",           "COV",       "Coventry City",
  8L,      "Crystal Palace",          "CRY",       "Crystal Palace",
  9L,      "Everton",                 "EVE",       "Everton",
  10L,      "Fulham",                  "FUL",       "Fulham",
  11L,      "Hull City",               "HUL",       "Hull City",
  12L,      "Ipswich Town",            "IPS",       "Ipswich Town",
  13L,      "Leeds United",            "LEE",       "Leeds United",
  14L,      "Liverpool",               "LIV",       "Liverpool",
  15L,      "Man City",                "MCI",       "Manchester City",
  16L,      "Man United",              "MUN",       "Manchester United",
  17L,      "Newcastle",               "NEW",       "Newcastle United",
  18L,      "Nott'm Forest",           "NFO",       "Nottingham Forest",
  19L,      "Sunderland",              "SUN",       "Sunderland",
  20L,      "Tottenham",               "TOT",       "Tottenham Hotspur"
)

stopifnot(nrow(teams) == 20,
          !anyDuplicated(teams$team_id),
          !anyDuplicated(teams$short_code),
          !anyDuplicated(teams$api_name))

# Badge asset check — every short_code must have its SVG in www/logos/
badge_files <- file.path("www", "logos",
                         paste0(tolower(teams$short_code), ".svg"))
missing_badges <- badge_files[!file.exists(badge_files)]
if (length(missing_badges) > 0) {
  stop(">>> [teams] missing badge asset(s): ",
       paste(missing_badges, collapse = ", "), call. = FALSE)
}
message("    [teams] all 20 badge assets present in www/logos/")

existing <- googlesheets4::sheet_names(sheet_id())
if ("teams" %in% existing) {
  stop(">>> [teams] `teams` tab already exists — refusing to overwrite. ",
       "Delete the tab in the browser first if a rewrite is intended.",
       call. = FALSE)
}

googlesheets4::sheet_write(teams, ss = sheet_id(), sheet = "teams")
message(">>> setup_pl_teams complete — wrote ", nrow(teams),
        " clubs to `teams`")