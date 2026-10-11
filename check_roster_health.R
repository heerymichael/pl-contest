# check_roster_health.R ------------------------------------------------------
#
# Read-only diagnostic. Writes NOTHING, anywhere.
#
# Checks every entry's roster against the players tab AS IT STANDS NOW, so
# that a refresh which deactivated, reclassified or moved a player shows up
# as the entry-level damage it actually causes.
#
# PERIOD-AWARE. roster_picks carries one full 15-player roster per period
# (period 1 = GW1-2, period 2 = GW3-19 after the transfer window). Each
# entry is judged on the roster that scores the CURRENT period, selected
# exactly as R/bestball.R's .bb_entry_roster() does for CHECK_PERIOD: the
# entry's rows tagged that period if any exist, otherwise ALL its rows
# (the tolerant fallback for entries that made no transfers). On top of
# that, every (entry, period) group must hold exactly 15 rows - a
# malformed historical period would misscore GW1-2 just as surely.
#
# Run after:  source("R/sheets.R")   (auth + SHEET_ID)
#             source("R/utils.R")    (%||%)
#
# Reports:
#   1. Every entry with a problem, and what the problem is
#   2. Ownership of every currently-inactive player
#
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(googlesheets4)
  library(dplyr)
})

message(">>> SOURCED check_roster_health.R at ", format(Sys.time()))

# --- Config ----------------------------------------------------------------

# Position quota, as enforced at entry time.
POS_TARGET <- c(GK = 2L, DEF = 5L, MID = 5L, FWD = 3L)

ROSTER_SIZE <- sum(POS_TARGET)

# The period whose roster is being validated. 2L from GW3 onward. Set to 1L
# to re-audit the locked GW1-2 rosters.
CHECK_PERIOD <- 2L

# Use the ID from R/sheets.R if it is loaded; fall back to the literal.
SHEET <- if (exists("SHEET_ID")) get("SHEET_ID") else
  "1oFdL_U-U4G-3YGXd13Pcb6s4-U01reZwEeVQ-ceUdI8"

if (!exists("%||%")) {
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
}

# --- Read ------------------------------------------------------------------
# Everything as character: player_id carries leading zeros and must never be
# coerced to numeric.

players <- range_read(SHEET, sheet = "players", col_types = "c")
picks   <- range_read(SHEET, sheet = "roster_picks", col_types = "c")
entries <- range_read(SHEET, sheet = "entries", col_types = "c")

message("    [health] players ", nrow(players),
        " | roster_picks ", nrow(picks),
        " | entries ", nrow(entries))

players <- players |>
  mutate(
    player_id = as.character(player_id),
    is_active = toupper(trimws(as.character(is_active))) %in% c("TRUE", "1")
  )

picks <- picks |>
  mutate(
    entry_id  = as.character(entry_id),
    player_id = as.character(player_id),
    period    = if ("period" %in% names(picks))
      suppressWarnings(as.integer(period)) else NA_integer_
  )

# --- Period structure ------------------------------------------------------

n_blank_period <- sum(is.na(picks$period))
period_sizes <- picks |>
  filter(!is.na(period)) |>
  count(entry_id, period, name = "n")
bad_period_sizes <- period_sizes |> filter(n != ROSTER_SIZE)

message("    [health] period rows: ",
        paste(sprintf("p%s=%d", names(table(picks$period)),
                      as.integer(table(picks$period))), collapse = ", "),
        " | blank period: ", n_blank_period,
        " | entries with period ", CHECK_PERIOD, ": ",
        length(unique(period_sizes$entry_id[period_sizes$period ==
                                              CHECK_PERIOD])))

# The roster that scores CHECK_PERIOD, per entry, mirroring
# .bb_entry_roster(): rows tagged CHECK_PERIOD if the entry has any,
# otherwise every row the entry holds.
has_period <- picks |>
  filter(period == CHECK_PERIOD) |>
  distinct(entry_id) |>
  pull(entry_id)

current <- bind_rows(
  picks |> filter(entry_id %in% has_period, period == CHECK_PERIOD),
  picks |> filter(!entry_id %in% has_period)
) |>
  mutate(roster_source = if_else(entry_id %in% has_period,
                                 paste0("period ", CHECK_PERIOD),
                                 "fallback (all rows)"))

entries <- entries |>
  mutate(entry_id = as.character(entry_id))

# Whatever the entries tab calls the human-readable fields, use the first
# match rather than assuming.
col_or_blank <- function(df, candidates) {
  hit <- intersect(candidates, names(df))
  if (length(hit) == 0) rep("", nrow(df)) else as.character(df[[hit[1]]])
}

entries_lookup <- tibble(
  entry_id   = entries$entry_id,
  who        = col_or_blank(entries, c("display_name", "user_name",
                                       "username", "user_email")),
  ename      = col_or_blank(entries, c("entry_name", "team_name", "name")),
  user_email = col_or_blank(entries, c("user_email", "email"))
)

# --- Join ------------------------------------------------------------------

joined <- current |>
  left_join(
    players |> select(player_id, name, team, position, is_active),
    by = "player_id"
  ) |>
  left_join(entries_lookup, by = "entry_id")

# --- Per-entry checks ------------------------------------------------------

report <- list()

for (eid in unique(joined$entry_id)) {
  
  rows  <- joined |> filter(entry_id == eid)
  who   <- rows$who[1] %||% ""
  ename <- rows$ename[1] %||% ""
  
  problems <- character(0)
  
  # Scoring-roster size. Guards against duplicate pick rows as well as
  # short rosters. For a fallback entry this is all its rows, so a stray
  # extra row shows up here too.
  if (nrow(rows) != ROSTER_SIZE) {
    problems <- c(problems,
                  paste0("ROSTER SIZE (", rows$roster_source[1], "): ",
                         nrow(rows), " pick rows, expected ", ROSTER_SIZE))
  }
  
  # Every period the entry holds must be a complete roster.
  bad_p <- bad_period_sizes |> filter(entry_id == eid)
  for (i in seq_len(nrow(bad_p))) {
    problems <- c(problems,
                  paste0("PERIOD ", bad_p$period[i], " SIZE: ", bad_p$n[i],
                         " rows, expected ", ROSTER_SIZE))
  }
  
  # A blank period cell falls outside every period filter - flag it.
  n_na <- sum(is.na(picks$period[picks$entry_id == eid]))
  if (n_na > 0) {
    problems <- c(problems, paste0("BLANK PERIOD: ", n_na, " row(s)"))
  }
  
  # Picks that no longer resolve to any row on the players tab at all.
  missing <- rows |> filter(is.na(name))
  if (nrow(missing) > 0) {
    problems <- c(problems,
                  paste0("UNKNOWN PLAYER_ID: ",
                         paste(missing$player_id, collapse = ", ")))
  }
  
  known <- rows |> filter(!is.na(name))
  
  # Owned players that are currently inactive. This is the one that silently
  # blanks a roster slot in the app.
  dead <- known |> filter(!is_active)
  if (nrow(dead) > 0) {
    problems <- c(problems,
                  paste0("INACTIVE PLAYER: ",
                         paste0(dead$name, " (", dead$team, ", ",
                                dead$position, ")", collapse = ", ")))
  }
  
  # One player per club, measured across the whole roster.
  clash <- known |>
    count(team, name = "n") |>
    filter(n > 1)
  if (nrow(clash) > 0) {
    for (i in seq_len(nrow(clash))) {
      dupes <- known |> filter(team == clash$team[i])
      problems <- c(problems,
                    paste0("CLUB CLASH at ", clash$team[i], ": ",
                           paste(dupes$name, collapse = " + ")))
    }
  }
  
  # Position quota, measured against the players tab as it stands now,
  # which is what a reclassification would have changed underneath them.
  counts <- table(factor(known$position, levels = names(POS_TARGET)))
  off <- names(POS_TARGET)[as.integer(counts[names(POS_TARGET)]) !=
                             POS_TARGET]
  off <- off[!is.na(off)]
  if (length(off) > 0) {
    detail <- paste0(off, " ", as.integer(counts[off]), "/",
                     POS_TARGET[off], collapse = ", ")
    problems <- c(problems, paste0("POSITION QUOTA: ", detail))
  }
  
  if (length(problems) > 0) {
    report[[length(report) + 1L]] <- list(
      entry_id = eid, who = who, ename = ename,
      email = rows$user_email[1] %||% "", problems = problems
    )
  }
}

# --- Output ----------------------------------------------------------------

message("")
message("=====================================================")
message(" ROSTER HEALTH — period ", CHECK_PERIOD, " — ",
        length(unique(joined$entry_id)),
        " entries checked (", length(has_period), " on period ",
        CHECK_PERIOD, ", ", length(unique(joined$entry_id)) -
          length(has_period), " on fallback), ",
        length(report), " with problems")
message("=====================================================")

if (length(report) == 0) {
  message(" All scoring rosters valid against the current players tab; ",
          "every period group holds ", ROSTER_SIZE, " rows.")
} else {
  for (r in report) {
    message("")
    message(" ", r$who, " — \"", r$ename, "\"")
    message("   entry_id: ", r$entry_id)
    for (p in r$problems) message("   - ", p)
  }
}

# --- Ownership of every currently-inactive player ---------------------------
# The headline number: of the players currently marked inactive, which are
# actually held by somebody, and by how many entries.

message("")
message("-----------------------------------------------------")
message(" INACTIVE PLAYERS BY OWNERSHIP (period ", CHECK_PERIOD, " rosters)")
message("-----------------------------------------------------")

owned_inactive <- joined |>
  filter(!is.na(name), !is_active) |>
  count(player_id, name, team, position, name = "entries_holding") |>
  arrange(desc(entries_holding), name)

if (nrow(owned_inactive) == 0) {
  message(" No inactive player is held by any entry.")
} else {
  for (i in seq_len(nrow(owned_inactive))) {
    message("   ", owned_inactive$name[i], " (", owned_inactive$team[i],
            ", ", owned_inactive$position[i], ") — held by ",
            owned_inactive$entries_holding[i], " entries")
  }
  message("")
  message(" TOTAL: ", nrow(owned_inactive), " inactive players held across ",
          length(unique(joined$entry_id[!is.na(joined$is_active) &
                                          !joined$is_active])),
          " entries")
}

# --- Self-verification -----------------------------------------------------

stopifnot(
  nrow(current) > 0,
  length(unique(current$entry_id)) == length(unique(picks$entry_id)),
  all(current$period[current$entry_id %in% has_period] == CHECK_PERIOD,
      na.rm = TRUE)
)

message("")
message(">>> check_roster_health complete — nothing written.")
