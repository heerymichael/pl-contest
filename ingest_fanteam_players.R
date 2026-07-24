# ingest_fanteam_players.R -----------------------------------------------
# One-time build of the `players` tab in pl-contest-data from a FanTeam
# 26/27 PL contest export.
#
# INPUT: data/fanteam_players_2627.csv — the raw FanTeam export with
# columns Tournament, PlayerID, Name, FName, Club, Lineup, Position,
# Price. Copy the downloaded export to that path before running.
#
# players tab columns written:
#   player_id     — contest-internal integer key (assigned here, stable
#                   from this point on; NEVER regenerate on re-ingest)
#   name          — display name ("FName Name"; single form where the
#                   two are identical — the WC doubled-name rule)
#   team          — contest short_code (FanTeam CVC mapped to COV;
#                   all others match); validated against the teams tab
#   position      — GK / DEF / MID / FWD
#   fanteam_id    — FanTeam's PlayerID, stored AS CHARACTER (ID string
#                   rule), for price refreshes and transfer updates
#   fanteam_price — price at ingest time; drives pool sort ONLY, never
#                   displayed in the UI
#   api_player_id — blank; filled by the API mapping pass (blank = the
#                   ingest gate treats the player as unmapped)
#
# Refuses to run if the players tab already exists — transfer-window
# updates will be a separate, additive script, not a rebuild (rebuilding
# would break roster_picks references to player_id).
#
# Run in two steps per the auth pattern:
#   source("R/sheets.R")
#   source("ingest_fanteam_players.R")

library(dplyr)
library(readr)
library(tibble)
library(googlesheets4)

message(">>> ingest_fanteam_players — target Sheet: ", sheet_id())

CSV_PATH <- "data/fanteam_players_2627.csv"

if (!file.exists(CSV_PATH)) {
  stop(">>> [players] input not found: ", CSV_PATH,
       " — copy the FanTeam export there first", call. = FALSE)
}

existing <- googlesheets4::sheet_names(sheet_id())
if ("players" %in% existing) {
  stop(">>> [players] `players` tab already exists — refusing to ",
       "overwrite (player_id stability). Transfer updates get their ",
       "own additive script.", call. = FALSE)
}

# --- Read + map ---------------------------------------------------------

raw <- read_csv(CSV_PATH,
                col_types = cols(.default = col_character(),
                                 Price = col_double()))
message("    [players] read ", nrow(raw), " rows from ", CSV_PATH)

POSITION_MAP <- c(goalkeeper = "GK", defender = "DEF",
                  midfielder = "MID", forward = "FWD")
CLUB_MAP     <- c(CVC = "COV")   # FanTeam quirks -> contest short_code

pos_in <- tolower(trimws(raw$Position))
bad_pos <- setdiff(unique(pos_in), names(POSITION_MAP))
if (length(bad_pos) > 0) {
  stop(">>> [players] unmapped position value(s): ",
       paste(bad_pos, collapse = ", "), call. = FALSE)
}

club_in <- toupper(trimws(raw$Club))
club_mapped <- ifelse(club_in %in% names(CLUB_MAP),
                      CLUB_MAP[club_in], club_in)

# Validate every club against the teams tab
teams <- read_sheet(sheet_id(), sheet = "teams",
                    col_types = "iccc")
bad_clubs <- setdiff(unique(club_mapped), teams$short_code)
if (length(bad_clubs) > 0) {
  stop(">>> [players] club code(s) not in teams tab: ",
       paste(bad_clubs, collapse = ", "),
       " — extend CLUB_MAP or fix the teams tab", call. = FALSE)
}

# Display name: "FName Name", collapsing the doubled-name case
# (FanTeam lists e.g. Gabriel/Gabriel) per the WC halving rule.
fn <- trimws(raw$FName)
sn <- trimws(raw$Name)
display_name <- ifelse(
  tolower(fn) == tolower(sn) & nzchar(fn),
  sn,
  trimws(paste(fn, sn))
)

players <- tibble(
  player_id     = seq_len(nrow(raw)),
  name          = display_name,
  team          = club_mapped,
  position      = unname(POSITION_MAP[pos_in]),
  fanteam_id    = as.character(raw$PlayerID),   # ID string rule
  fanteam_price = raw$Price,
  api_player_id = ""
) |>
  arrange(desc(fanteam_price), name) |>
  mutate(player_id = row_number())   # IDs follow pool order at birth

# --- Sanity report ------------------------------------------------------

stopifnot(!anyDuplicated(players$fanteam_id))

message("    [players] ", nrow(players), " players; positions: ",
        paste(names(table(players$position)), table(players$position),
              sep = "=", collapse = ", "))
per_club <- table(players$team)
message("    [players] per club: min ", min(per_club),
        " (", names(per_club)[which.min(per_club)], "), max ",
        max(per_club), " (", names(per_club)[which.max(per_club)], ")")

# Any name appearing twice in the same club deserves eyeballs (genuine
# duplicates vs distinct players sharing a name — WC lesson)
dupes <- players |> count(team, name) |> filter(n > 1)
if (nrow(dupes) > 0) {
  message("!!! [players] same name twice within a club — REVIEW:")
  for (i in seq_len(nrow(dupes))) {
    message("    - ", dupes$team[i], " ", dupes$name[i],
            " (x", dupes$n[i], ")")
  }
} else {
  message("    [players] no within-club duplicate names")
}

# --- Write --------------------------------------------------------------

googlesheets4::sheet_write(players, ss = sheet_id(), sheet = "players")
message(">>> ingest_fanteam_players complete — wrote ", nrow(players),
        " players to `players`")