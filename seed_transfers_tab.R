# seed_transfers_tab.R -------------------------------------------------------
# One-off: create the `transfers` tab on pl-contest-data — the permanent
# audit log of roster changes — and seed it with the two rows recording
# the 23 Aug Konsa -> Cash commissioner correction.
#
# The tab is the single source for the squad panel's Transfers view
# (R/leaderboard.R) and will be appended to by apply_transfers() when the
# September window closes (TRANSFER_WINDOW.md, Batch 5) and by any future
# commissioner correction. One tab, three event types
# (commissioner / window / autosub), ids not names — names resolve at
# render time from the players tab.
#
# Columns: transfer_id, entry_id, out_player_id, in_player_id, type,
#          effective_gw, note, created_at   (all written as text except
#          effective_gw, which lands as a number)
#
# BACKGROUND. Ezri Konsa (player_id 210) moved AVL -> ARS before the
# 23 Aug FanTeam refresh. Two entries held him alongside another ARS
# player; per the one-club rule and commissioner ruling (transfer
# predated the data refresh, so the lag was corrected rather than carried)
# both had player 210 replaced in place by Matty Cash (player_id 153,
# top-priced active AVL DEF) with effect from GW1. The roster edit is
# already live; this script records it.
#
# GATES (script hard-stops if any fail):
#   - The two known pick_ids exist, belong to 2 distinct entries, and now
#     hold player 153 (i.e. the roster edit this log records has actually
#     been made).
#   - The transfers tab does not already exist (or, if FORCE_APPEND, the
#     seed rows are not already present) — idempotence guard.
#
# Safety: DRY_RUN is TRUE by default. A dry run performs every read and
# gate and prints the proposed rows — nothing live is touched. Review,
# then EDIT THE FLAG BELOW IN THIS FILE and re-run. (Console assignment
# of DRY_RUN is overwritten on source() — the OWN_GOAL_ALLOWANCE lesson.)
# Flip it back to TRUE after the live run.
#
# Usage:
#   1. source("R/sheets.R")                 # let auth complete first
#   2. source("seed_transfers_tab.R")       # dry run — review output
#   3. Edit DRY_RUN below to FALSE, re-run to commit, flip back to TRUE.
# ---------------------------------------------------------------------------

DRY_RUN <- TRUE

# The two roster_picks rows edited on 23 Aug (player_id 210 -> 153).
SWAP_PICK_IDS <- c("28e35b17-64fc-4a6c-a65c-db8f30959b38",
                   "b3b47132-ea12-4af0-a741-9b4b39d70eab")

OUT_PLAYER_ID <- "210"   # Ezri Konsa
IN_PLAYER_ID  <- "153"   # Matty Cash
EFFECTIVE_GW  <- 1L
TYPE          <- "commissioner"
NOTE          <- paste(
  "Konsa joined Arsenal before the GW1 data refresh;",
  "corrected per the one-club rule."
)

library(dplyr)
library(googlesheets4)

message(">>> seed_transfers_tab — target Sheet: ", sheet_id(),
        if (DRY_RUN) "  [DRY RUN]" else "  [LIVE WRITE]")

# --- Gate 1: the roster edit this log records has actually been made -------

picks <- read_sheet(sheet_id(), sheet = "roster_picks", col_types = "c")
message("    [seed] read roster_picks — ", nrow(picks), " rows")

swap_rows <- picks |> filter(pick_id %in% SWAP_PICK_IDS)
if (nrow(swap_rows) != length(SWAP_PICK_IDS)) {
  stop(">>> [seed] expected ", length(SWAP_PICK_IDS), " pick row(s), found ",
       nrow(swap_rows), " — pick_ids don't match the tab", call. = FALSE)
}
if (length(unique(swap_rows$entry_id)) != 2L) {
  stop(">>> [seed] the two picks do not belong to 2 distinct entries",
       call. = FALSE)
}
if (!all(swap_rows$player_id == IN_PLAYER_ID)) {
  stop(">>> [seed] pick row(s) do not hold player ", IN_PLAYER_ID,
       " — the roster correction this log records has not been made ",
       "(found: ", paste(swap_rows$player_id, collapse = ", "), ")",
       call. = FALSE)
}
message("    [seed] gate 1 OK — both picks hold player ", IN_PLAYER_ID,
        " across 2 distinct entries")

# --- Gate 2: idempotence ---------------------------------------------------

existing <- tryCatch(read_sheet(sheet_id(), sheet = "transfers",
                                col_types = "c"),
                     error = function(e) NULL)
tab_exists <- !is.null(existing)

if (tab_exists && nrow(existing) > 0) {
  dup <- existing |>
    filter(entry_id %in% swap_rows$entry_id,
           out_player_id == OUT_PLAYER_ID,
           in_player_id  == IN_PLAYER_ID)
  if (nrow(dup) > 0) {
    stop(">>> [seed] ", nrow(dup), " matching row(s) already on the ",
         "transfers tab — already seeded, nothing to do", call. = FALSE)
  }
}
message("    [seed] gate 2 OK — ",
        if (!tab_exists) "transfers tab does not exist yet, will be created"
        else paste0("tab exists (", nrow(existing),
                    " rows), seed rows not present"))

# --- Build -----------------------------------------------------------------

now_str <- format(lubridate::with_tz(Sys.time(), "Europe/London"),
                  "%Y-%m-%d %H:%M:%S")

seed_rows <- tibble::tibble(
  transfer_id   = vapply(seq_along(swap_rows$entry_id),
                         function(i) uuid::UUIDgenerate(), character(1)),
  entry_id      = swap_rows$entry_id,
  out_player_id = OUT_PLAYER_ID,
  in_player_id  = IN_PLAYER_ID,
  type          = TYPE,
  effective_gw  = EFFECTIVE_GW,
  note          = NOTE,
  created_at    = now_str
)

message("    [seed] proposed rows:")
for (i in seq_len(nrow(seed_rows))) {
  message("      entry ", substr(seed_rows$entry_id[i], 1, 8),
          "\u2026  OUT ", OUT_PLAYER_ID, " -> IN ", IN_PLAYER_ID,
          "  [", TYPE, ", effective GW", EFFECTIVE_GW, "]")
}

# --- Write (or review) -----------------------------------------------------

if (DRY_RUN) {
  message(">>> [seed] DRY RUN complete — nothing written to the Sheet. ",
          "Edit DRY_RUN to FALSE and re-run to commit.")
} else {
  if (!tab_exists) {
    sheet_write(seed_rows, ss = sheet_id(), sheet = "transfers")
    message("    [seed] created tab 'transfers' with ", nrow(seed_rows),
            " rows")
  } else {
    sheet_append(sheet_id(), seed_rows, sheet = "transfers")
    message("    [seed] appended ", nrow(seed_rows), " rows")
  }
  
  chk <- read_sheet(sheet_id(), sheet = "transfers", col_types = "c")
  got <- chk |>
    filter(transfer_id %in% seed_rows$transfer_id)
  if (nrow(got) != nrow(seed_rows)) {
    stop(">>> [seed] VERIFICATION FAILED — ", nrow(got), " of ",
         nrow(seed_rows), " seed rows read back; inspect the tab by hand",
         call. = FALSE)
  }
  message(">>> [seed] LIVE WRITE complete and verified — transfers tab ",
          "holds ", nrow(chk), " row(s). The squad panel Transfers view ",
          "picks this up within 5 minutes (cached read). ",
          "Flip DRY_RUN back to TRUE in this file.")
}
