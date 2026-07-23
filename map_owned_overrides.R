# map_owned_overrides.R --------------------------------------------------------
# One-off: write known api_player_id mappings into column H of the players tab
# for owned players the auto-matcher won't guess (nicknames / bare first names).
#
# These three were verified against the mt_153637518 (Brazil vs Morocco)
# player-stats payload — team + minutes confirm the identity, so there is no
# two-Danilo ambiguity:
#   142 Danilo Luiz (BRA, DEF) -> pl_9760160  (API "Danilo",   45')  NOT pl_74112545 "Danilo Oliveira" (10', your Danilo Santos)
#   144 Gabriel     (BRA, DEF) -> pl_96242662 (API "Gabriel Magalhães", 90')  distinct from Gabriel Martinelli
#   767 Yassine Bounou (MAR, GK) -> pl_8642423 (API "Bono",     90')
#
# Keyed by player_id so the correct rows are hit regardless of sheet order.
# Never clobbers an existing DIFFERENT mapping — conflicts are reported, skipped.
#
# DRY RUN FIRST: with DRY_RUN = TRUE (default) it only prints the diff. Review,
# set DRY_RUN <- FALSE, re-run to write. Then re-run the stats update.
#
# Run with (from the PROJECT ROOT):
#   source("map_owned_overrides.R")
# ------------------------------------------------------------------------------

DRY_RUN <- FALSE

message(">>> SOURCED map_owned_overrides.R (v1) at ",
        format(Sys.time(), "%Y-%m-%d %H:%M:%S"))

source("R/sheets.R")   # gs_auth(), read_sheet_tab(), sheet_id()

# --- The overrides ------------------------------------------------------------

OVERRIDES <- data.frame(
  player_id     = c(142, 144, 767),
  api_player_id = c("pl_9760160", "pl_96242662", "pl_8642423"),
  label         = c("Danilo Luiz (BRA, DEF) <- API 'Danilo'",
                    "Gabriel (BRA, DEF) <- API 'Gabriel Magalhaes'",
                    "Yassine Bounou (MAR, GK) <- API 'Bono'"),
  stringsAsFactors = FALSE
)

# --- Read, apply, report ------------------------------------------------------

gs_auth()
players <- read_sheet_tab("players")
message(">>> [sheets] players tab: ", nrow(players), " rows")

if (!"api_player_id" %in% names(players)) {
  players$api_player_id <- NA_character_
} else {
  players$api_player_id <- as.character(players$api_player_id)
}

pid_chr <- as.character(players$player_id)
n_write <- 0L; n_conflict <- 0L; n_missing <- 0L; n_already <- 0L

for (i in seq_len(nrow(OVERRIDES))) {
  pid  <- as.character(OVERRIDES$player_id[i])
  newv <- OVERRIDES$api_player_id[i]
  row  <- which(pid_chr == pid)
  
  if (length(row) != 1L) {
    message(">>> [override] player_id ", pid, " — NOT FOUND (", length(row),
            " matches) — skipped")
    n_missing <- n_missing + 1L
    next
  }
  
  cur  <- players$api_player_id[row]
  nm   <- players$name[row]
  curd <- if (is.na(cur) || !nzchar(cur)) "<empty>" else cur
  
  if (!is.na(cur) && nzchar(cur) && !identical(cur, newv)) {
    message(">>> [override] player_id ", pid, "  ", nm,
            "  *** CONFLICT *** current H: ", curd,
            "  (would NOT overwrite with ", newv, ") — skipped")
    n_conflict <- n_conflict + 1L
    next
  }
  
  if (identical(cur, newv)) {
    message(">>> [override] player_id ", pid, "  ", nm,
            "  already set to ", newv, " — no change")
    n_already <- n_already + 1L
    next
  }
  
  players$api_player_id[row] <- newv
  n_write <- n_write + 1L
  message(">>> [override] player_id ", pid, "  ", nm,
          "  current H: ", curd, "  ->  ", newv,
          "   [", OVERRIDES$label[i], "]")
}

message(">>> [override] summary: ", n_write, " to write, ", n_already,
        " already set, ", n_conflict, " conflict(s), ", n_missing, " not found")

# --- Write column H back ------------------------------------------------------

if (n_conflict > 0) {
  message(">>> ABORTING — unexpected conflict(s) above. Resolve before writing.")
} else if (n_write == 0L) {
  message(">>> Nothing to write — Sheet untouched.")
} else if (DRY_RUN) {
  message(">>> DRY RUN — set DRY_RUN <- FALSE and re-run to write column H")
} else {
  message(">>> [sheets] writing api_player_id column (", n_write,
          " mapping(s)) to players tab column H")
  col_df <- data.frame(api_player_id = players$api_player_id)
  range_write(
    sheet_id(),
    data      = col_df,
    sheet     = "players",
    range     = paste0("H1:H", nrow(players) + 1L),
    col_names = TRUE,
    reformat  = FALSE
  )
  message(">>> [sheets] write complete")
  message(">>> NEXT: re-run the stats update to ingest the now-unblocked match:")
  message(">>>   source(\"run_stats_update.R\")")
}

message(">>> [override] done at ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
invisible(NULL)