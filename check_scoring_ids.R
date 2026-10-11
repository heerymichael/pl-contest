# check_scoring_ids.R --------------------------------------------------------
#
# Read-only diagnostic. Writes NOTHING to Sheets. Optionally writes one
# local CSV to feed the mapping fix.
#
# Answers one question: can every player somebody actually owns be linked
# to their match stats?
#
# The bridge is players.api_player_id — the TheStatsAPI player id ("pl_...").
# Without it a player's stat rows cannot be resolved back to a contest
# player_id, so they score nothing, silently, for whoever holds them. The
# app logs dropped rows but the entrant just sees a zero.
#
# Pool-wide coverage is NOT the number that matters. ~35% of the pool being
# unmapped is survivable if none of those players are owned; one unmapped
# player owned by six entries is not. This script reports the owned figure.
#
# Run after:  source("R/sheets.R")   (auth + SHEET_ID)
#
# Reports:
#   1. Coverage: pool-wide, and across owned players only
#   2. Every OWNED player with no api_player_id, with ownership count
#   3. Duplicate api_player_id — two contest players pointing at one API
#      player, which makes one of them steal the other's stats
#   4. Club-level breakdown of the owned gap
#
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(googlesheets4)
  library(dplyr)
})

message(">>> SOURCED check_scoring_ids.R at ", format(Sys.time()))

# --- Config ----------------------------------------------------------------

# Write the unmapped-owned list to disk for the mapping step to consume.
# Local file only — no Sheets write happens anywhere in this script.
WRITE_CSV <- TRUE
CSV_PATH  <- "data/unmapped_owned_players.csv"

# Voided (unpaid) entries — excluded from the leaderboard, so a player
# owned ONLY by a void entry is not a scoring problem. Keep in step with
# VOID_ENTRY_IDS in R/leaderboard.R.
VOID_ENTRY_IDS <- c(
  "72ee937b-0d41-4d5f-bc03-002482ca021d",
  "863344bd-51cc-41c6-9e1d-c22ed70b5068"
)

SHEET <- if (exists("SHEET_ID")) get("SHEET_ID") else
  "1oFdL_U-U4G-3YGXd13Pcb6s4-U01reZwEeVQ-ceUdI8"

# --- Read ------------------------------------------------------------------
# Everything as character — player_id must never be coerced to numeric.

players <- range_read(SHEET, sheet = "players",      col_types = "c")
picks   <- range_read(SHEET, sheet = "roster_picks", col_types = "c")
entries <- range_read(SHEET, sheet = "entries",      col_types = "c")

message("    [ids] players ", nrow(players),
        " | roster_picks ", nrow(picks),
        " | entries ", nrow(entries))

if (!"api_player_id" %in% names(players)) {
  stop("players tab has no api_player_id column — nothing to check.",
       call. = FALSE)
}

blank <- function(x) {
  x <- trimws(as.character(x))
  is.na(x) | !nzchar(x) | toupper(x) %in% c("NA", "NULL", "-")
}

players <- players |>
  mutate(
    player_id     = as.character(player_id),
    api_player_id = trimws(as.character(api_player_id)),
    mapped        = !blank(api_player_id),
    is_active     = if ("is_active" %in% names(players))
      toupper(trimws(as.character(is_active))) %in%
      c("TRUE", "1") else TRUE
  )

picks <- picks |>
  mutate(
    entry_id  = as.character(entry_id),
    player_id = as.character(player_id)
  )

entries <- entries |> mutate(entry_id = as.character(entry_id))

live_picks <- picks |> filter(!entry_id %in% VOID_ENTRY_IDS)

# --- 1. Coverage -----------------------------------------------------------

owned_ids <- sort(unique(live_picks$player_id))
owned <- players |> filter(player_id %in% owned_ids)

# Picks pointing at a player_id that isn't on the players tab at all —
# a different and worse problem than a missing api id.
ghost_ids <- setdiff(owned_ids, players$player_id)

message("")
message(">>> [ids] ============ COVERAGE ============")
message(">>> [ids] pool          : ", nrow(players), " rows, ",
        sum(players$mapped), " mapped (",
        round(100 * mean(players$mapped)), "%)")
message(">>> [ids] pool (active) : ", sum(players$is_active), " rows, ",
        sum(players$mapped & players$is_active), " mapped (",
        round(100 * mean(players$mapped[players$is_active])), "%)")
message(">>> [ids] OWNED         : ", length(owned_ids), " distinct players ",
        "across ", nrow(live_picks), " live picks / ",
        length(unique(live_picks$entry_id)), " entries")
message(">>> [ids] OWNED mapped  : ", sum(owned$mapped), " of ", nrow(owned),
        " (", if (nrow(owned) > 0)
          round(100 * mean(owned$mapped)) else 0, "%)")

if (length(ghost_ids) > 0) {
  message("")
  message(">>> [ids] !!! ", length(ghost_ids), " owned player_id(s) are NOT ",
          "on the players tab at all: ",
          paste(utils::head(ghost_ids, 10), collapse = ", "))
  message(">>> [ids] !!! These cannot score under any mapping. Investigate ",
          "before anything else.")
}

# --- 2. Unmapped owned players ---------------------------------------------

own_counts <- live_picks |>
  count(player_id, name = "owned_by")

gap <- owned |>
  filter(!mapped) |>
  left_join(own_counts, by = "player_id") |>
  arrange(desc(owned_by), team, name) |>
  select(any_of(c("player_id", "name", "team", "position", "owned_by",
                  "is_active", "fanteam_price")))

message("")
message(">>> [ids] ======= UNMAPPED OWNED PLAYERS =======")
if (nrow(gap) == 0) {
  message(">>> [ids] NONE — every owned player has an api_player_id.")
} else {
  message(">>> [ids] ", nrow(gap), " owned player(s) have no api_player_id, ",
          "affecting ", sum(gap$owned_by), " pick(s).")
  message("")
  print(as.data.frame(gap), row.names = FALSE)
  
  # How many entries are touched at all — the number that matters for
  # deciding whether this blocks the first ingest.
  hit_entries <- live_picks |>
    filter(player_id %in% gap$player_id) |>
    distinct(entry_id) |>
    nrow()
  message("")
  message(">>> [ids] ", hit_entries, " of ",
          length(unique(live_picks$entry_id)),
          " live entries hold at least one unmappable player.")
}

# --- 3. Duplicate api_player_id --------------------------------------------
# Two contest players pointing at the same API player means one set of
# stats is claimed twice and the other player scores nothing. Silent and
# very hard to spot from the board.

dups <- players |>
  filter(mapped) |>
  count(api_player_id, name = "n") |>
  filter(n > 1)

message("")
message(">>> [ids] ======= DUPLICATE API IDS =======")
if (nrow(dups) == 0) {
  message(">>> [ids] none — every api_player_id is used at most once.")
} else {
  message(">>> [ids] !!! ", nrow(dups), " api_player_id(s) used more than ",
          "once — one player will steal the other's stats:")
  detail <- players |>
    filter(api_player_id %in% dups$api_player_id) |>
    arrange(api_player_id, name) |>
    mutate(owned = player_id %in% owned_ids) |>
    select(any_of(c("api_player_id", "player_id", "name", "team",
                    "position", "owned")))
  print(as.data.frame(detail), row.names = FALSE)
}

# --- 4. Club breakdown of the owned gap ------------------------------------

if (nrow(gap) > 0 && "team" %in% names(gap)) {
  message("")
  message(">>> [ids] ======= OWNED GAP BY CLUB =======")
  by_club <- gap |>
    count(team, name = "unmapped_owned") |>
    arrange(desc(unmapped_owned), team)
  print(as.data.frame(by_club), row.names = FALSE)
}

# --- Output ----------------------------------------------------------------

if (WRITE_CSV && nrow(gap) > 0) {
  dir.create(dirname(CSV_PATH), showWarnings = FALSE, recursive = TRUE)
  write.csv(gap, CSV_PATH, row.names = FALSE, na = "")
  message("")
  message(">>> [ids] wrote ", nrow(gap), " rows to ", CSV_PATH,
          " (local file — no Sheets write)")
}

message("")
if (nrow(gap) == 0 && length(ghost_ids) == 0 && nrow(dups) == 0) {
  message(">>> [ids] ===== CLEAR TO INGEST =====")
} else {
  message(">>> [ids] ===== GAPS PRESENT — see above before ingesting =====")
}