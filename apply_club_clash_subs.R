# apply_club_clash_subs.R ----------------------------------------------------
# Club-clash automatic substitutions after the 2 Sep 2026 pool refresh.
#
# THE RULE (Rules page, "Club clashes"): a player who transfers to a club
# where the entry already holds a player is dropped and replaced by the
# next listed player in his position at the club he left. Listing order is
# the pool's own: fanteam_price descending, then name. Forced, not chosen;
# does not count against the entry's two window transfers.
#
# WHAT IT WRITES (live run only):
#   roster_picks  15 new rows per affected entry, period = 2 — the entry's
#                 full roster with the mover(s) swapped for the
#                 replacement(s). Period-1 rows are NEVER touched, so GW1-2
#                 points for the dropped player stand.
#   transfers     one row per substitution, type = "autosub",
#                 effective_gw = 3, ids not names.
#
# Both appends happen in one run so a roster change can never exist
# without its log row (or vice versa). Entries with no clash get NO
# period-2 rows here — bestball falls back to period 1 for them, and
# apply_transfers() will seed period 2 for any entry that transfers.
#
# MOVERS and their REPLACEMENTS are pinned explicitly below rather than
# derived, so the script is auditable and re-runnable against the same
# facts. Gate 1 confirms the players tab agrees with the moves; gate 4
# confirms each replacement really is "next listed" (active, old club,
# same position, nobody active between him and the mover on price).
#
# TIE-BREAK (confirmed 2 Sep 2026): the pool's listing order decides —
# fanteam_price descending, then name ascending — exactly as the Rules
# page states. Muñoz -> Axel Disasi (first of six CRY DEF at £4.5);
# Ndiaye -> Brennan Johnson (first of two EVE MID at £5.5). Suzuki and
# Neto were unambiguous.
#
# GATES (hard-stop if any fail):
#   1. Every mover's team on the players tab == new_team (refresh is live)
#   2. roster_picks: every row has period == 1, none has period == 2
#      (Phase 1 done; this script has not already run)
#   3. transfers: no existing autosub rows with effective_gw == 3
#   4. Every pinned replacement is active, at the old club, same position,
#      and in the next price band below the mover
#   5. Every rebuilt roster: 15 rows, 2/5/5/3, one player per club
#
# Safety: DRY_RUN is TRUE by default. A dry run performs every read and
# gate, prints the full substitution table and writes the proposed rows
# to /tmp for review. Nothing live is touched. Flip with
#   set_dry_run("apply_club_clash_subs.R", FALSE)
# (console assignment is overwritten on source()); flip back after.
#
# Usage:
#   source("R/sheets.R")                   # auth, selection 2
#   source("apply_club_clash_subs.R")      # dry run — review
#   set_dry_run("apply_club_clash_subs.R", FALSE); source(...); set_dry_run(..., TRUE)
# ---------------------------------------------------------------------------

DRY_RUN <- TRUE

EFFECTIVE_GW <- 3L
TYPE         <- "autosub"

MOVERS <- tibble::tribble(
  ~player_id, ~name,               ~old_team, ~new_team, ~in_player_id, ~in_name,
  "200",      "Daniel Muñoz",      "CRY",     "NFO",     "316",         "Axel Disasi",
  "50",       "Iliman Ndiaye",     "EVE",     "MCI",     "115",         "Brennan Johnson",
  "207",      "Emiliano Martínez", "AVL",     "CHE",     "616",         "Zion Suzuki",
  "15",       "Enzo Fernández",    "CHE",     "MCI",     "27",          "Pedro Neto"
)

REVIEW_PICKS_CSV     <- "/tmp/club_clash_subs_period2_picks.csv"
REVIEW_TRANSFERS_CSV <- "/tmp/club_clash_subs_transfers.csv"

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(googlesheets4)
})

message(">>> apply_club_clash_subs — target Sheet: ", sheet_id(),
        if (DRY_RUN) "  [DRY RUN]" else "  [LIVE WRITE]")

if (!exists("sheet_id")) {
  stop("source(\"R/sheets.R\") first: sheet_id() and auth are required.",
       call. = FALSE)
}

is_true_text <- function(x) toupper(trimws(as.character(x))) %in% c("TRUE", "1")

# --- Read (all as character; coerce explicitly) ----------------------------

players <- read_sheet(sheet_id(), sheet = "players", col_types = "c") |>
  mutate(player_id     = trimws(player_id),
         fanteam_price = suppressWarnings(as.numeric(fanteam_price)),
         is_active     = is_true_text(is_active))
message("    [subs] players ", nrow(players), " rows, ",
        sum(players$is_active), " active")

picks <- read_sheet(sheet_id(), sheet = "roster_picks", col_types = "c") |>
  mutate(player_id = trimws(player_id),
         period    = suppressWarnings(as.integer(period)))
message("    [subs] roster_picks ", nrow(picks), " rows")

entries <- read_sheet(sheet_id(), sheet = "entries", col_types = "c")
message("    [subs] entries ", nrow(entries), " rows")

transfers <- tryCatch(
  read_sheet(sheet_id(), sheet = "transfers", col_types = "c"),
  error = function(e) stop(">>> [subs] transfers tab unreadable — seed it ",
                           "first (seed_transfers_tab.R)", call. = FALSE)
)
message("    [subs] transfers ", nrow(transfers), " rows")

# --- Gate 1: players tab reflects the moves --------------------------------

mv <- MOVERS |>
  left_join(players |> select(player_id, tab_team = team, position,
                              fanteam_price),
            by = "player_id")
bad <- mv |> filter(is.na(tab_team) | tab_team != new_team)
if (nrow(bad) > 0) {
  stop(">>> [subs] GATE 1 FAILED — players tab disagrees with MOVERS for: ",
       paste0(bad$name, " (tab=", bad$tab_team, ", expected ", bad$new_team,
              ")", collapse = "; "),
       ". Has refresh_fanteam_players.R run live?", call. = FALSE)
}
message("    [subs] gate 1 OK — all ", nrow(mv), " movers at their new club")

# --- Gate 2: period-1 complete, no period-2 yet ---------------------------

n_p1 <- sum(!is.na(picks$period) & picks$period == 1L)
n_p2 <- sum(!is.na(picks$period) & picks$period == 2L)
if (n_p1 != nrow(picks)) {
  stop(">>> [subs] GATE 2 FAILED — ", nrow(picks) - n_p1,
       " roster_picks row(s) not period 1 (blank or other). ",
       "Complete Phase 1 first.", call. = FALSE)
}
if (n_p2 > 0) {
  stop(">>> [subs] GATE 2 FAILED — ", n_p2, " period-2 row(s) already ",
       "exist. This script has run, or the window has opened. Stop.",
       call. = FALSE)
}
message("    [subs] gate 2 OK — ", n_p1, " period-1 rows, 0 period-2 rows")

# --- Gate 3: transfers idempotence ----------------------------------------

if (nrow(transfers) > 0) {
  prior <- transfers |>
    filter(tolower(trimws(type)) == TYPE,
           suppressWarnings(as.integer(effective_gw)) == EFFECTIVE_GW)
  if (nrow(prior) > 0) {
    stop(">>> [subs] GATE 3 FAILED — ", nrow(prior), " '", TYPE,
         "' row(s) effective GW", EFFECTIVE_GW, " already on the ",
         "transfers tab. Already run.", call. = FALSE)
  }
}
message("    [subs] gate 3 OK — no prior ", TYPE, " rows for GW", EFFECTIVE_GW)

# --- Gate 4: pinned replacements are legitimate "next listed" ------------

# Listing order = pool order: price desc, then name. The replacement must
# be active, at the mover's old club, in his position, and in the next
# price band: no active same-position player at that club may sit strictly
# between the mover's price and the replacement's price.
mv <- mv |>
  left_join(players |> select(player_id, in_tab_name = name, in_team = team,
                              in_position = position, in_price = fanteam_price,
                              in_active = is_active),
            by = c("in_player_id" = "player_id"))

for (i in seq_len(nrow(mv))) {
  m <- mv[i, ]
  fail <- function(why) stop(">>> [subs] GATE 4 FAILED — ", m$name, " -> ",
                             m$in_name, ": ", why, call. = FALSE)
  if (is.na(m$in_tab_name))             fail("in_player_id not on players tab")
  if (m$in_tab_name != m$in_name)       fail(paste0("id resolves to ", m$in_tab_name))
  if (!isTRUE(m$in_active))             fail("replacement is inactive")
  if (m$in_team != m$old_team)          fail(paste0("replacement at ", m$in_team,
                                                    ", not ", m$old_team))
  if (m$in_position != m$position)      fail(paste0("position ", m$in_position,
                                                    " != ", m$position))
  if (m$in_price > m$fanteam_price)     fail("replacement priced above mover")
  between <- players |>
    filter(team == m$old_team, position == m$position, is_active,
           player_id != m$player_id,
           fanteam_price < m$fanteam_price, fanteam_price > m$in_price)
  if (nrow(between) > 0)
    fail(paste0("skips a higher-listed player: ",
                paste(between$name, collapse = ", ")))
  if (m$in_price == m$fanteam_price && m$in_name < m$name)
    fail("same price as mover but listed above him")
  tied_above <- players |>
    filter(team == m$old_team, position == m$position, is_active,
           player_id != m$in_player_id, player_id != m$player_id,
           fanteam_price == m$in_price, name < m$in_name)
  if (nrow(tied_above) > 0)
    fail(paste0("not first by name at this price: ",
                paste(tied_above$name, collapse = ", "), " list above"))
}
message("    [subs] gate 4 OK — replacements:")
for (i in seq_len(nrow(mv))) {
  tied <- players |>
    filter(team == mv$old_team[i], position == mv$position[i], is_active,
           fanteam_price == mv$in_price[i], player_id != mv$in_player_id[i]) |>
    nrow()
  message("      ", mv$name[i], " (", mv$old_team[i], " ", mv$position[i],
          " \u00a3", mv$fanteam_price[i], ") -> ", mv$in_name[i],
          " (\u00a3", mv$in_price[i], ")",
          if (tied > 0) paste0("  [", tied, " other(s) at this price — ",
                               "first by name]") else "")
}

# --- Find clashes (iterative, per entry) -----------------------------------

# A substitution can itself create a clash: swapping a mover for a player
# at his OLD club collides with a second mover who has just ARRIVED at
# that club (Louie: Enzo -> Neto lands at CHE where Martínez now sits).
# So the search is per entry and repeats — apply one substitution, look
# again — until the roster is clash-free. The transferring player always
# drops. Movers are considered in MOVERS order for determinism.

p1 <- picks |>
  left_join(players |> select(player_id, name, team, position),
            by = "player_id") |>
  left_join(entries |> select(entry_id, entry_name, display_name),
            by = "entry_id")

subs      <- list()
p2_roster <- list()   # entry_id -> post-substitution roster (p1 columns)

for (eid in unique(p1$entry_id)) {
  roster <- p1 |> filter(entry_id == eid)
  guard  <- 0L
  repeat {
    guard <- guard + 1L
    if (guard > 10L) stop(">>> [subs] runaway substitution loop for entry ",
                          eid, call. = FALSE)
    hit <- NULL
    for (i in seq_len(nrow(mv))) {
      m <- mv[i, ]
      if (!m$player_id %in% roster$player_id) next
      others <- roster |>
        filter(player_id != m$player_id, team == m$new_team)
      if (nrow(others) > 0) { hit <- list(m = m, others = others); break }
    }
    if (is.null(hit)) break
    m <- hit$m
    subs[[length(subs) + 1L]] <- tibble(
      entry_id      = eid,
      entry_name    = roster$entry_name[1],
      display_name  = roster$display_name[1],
      out_player_id = m$player_id,
      out_name      = m$name,
      clash_club    = m$new_team,
      clash_with    = paste(hit$others$name, collapse = ", "),
      in_player_id  = m$in_player_id,
      in_name       = m$in_name
    )
    k <- which(roster$player_id == m$player_id)
    roster$player_id[k] <- m$in_player_id
    roster$name[k]      <- m$in_name
    roster$team[k]      <- m$old_team
    roster$position[k]  <- m$position
  }
  if (guard > 1L) p2_roster[[eid]] <- roster
}
subs <- bind_rows(subs)

message("\n=== SUBSTITUTIONS: ", nrow(subs), " across ",
        length(unique(subs$entry_id)), " entries ===")
if (nrow(subs) > 0) {
  print(subs |>
          select(entry_name, display_name, out_name, clash_club, clash_with,
                 in_name) |>
          as.data.frame(), row.names = FALSE)
  message("\n    by substitution:")
  cnt <- subs |> count(out_name, in_name)
  for (i in seq_len(nrow(cnt))) {
    message("      ", cnt$out_name[i], " -> ", cnt$in_name[i], "  x",
            cnt$n[i])
  }
  multi <- subs |> count(entry_name, display_name) |> filter(n > 1)
  if (nrow(multi) > 0) {
    message("\n    entries with more than one substitution:")
    for (i in seq_len(nrow(multi))) {
      message("      ", multi$entry_name[i], " (", multi$display_name[i],
              ")  x", multi$n[i])
    }
  }
}
if (nrow(subs) == 0) {
  message(">>> [subs] nothing to do."); stop_quietly <- TRUE
} else stop_quietly <- FALSE

if (!stop_quietly) {
  
  # --- Build period-2 rosters -----------------------------------------------
  
  now      <- Sys.time()
  now_str  <- format(lubridate::with_tz(now, "Europe/London"),
                     "%Y-%m-%d %H:%M:%S")
  
  affected <- unique(subs$entry_id)
  p2_rows  <- list()
  for (eid in affected) {
    # Re-resolve club/position from the players tab on the substituted roster.
    roster <- p2_roster[[eid]] |>
      select(pick_id, entry_id, player_id, slot, created_at, period) |>
      left_join(players |> select(player_id, team, position), by = "player_id")
    
    # Gate 5: roster validity.
    if (nrow(roster) != 15L) {
      stop(">>> [subs] GATE 5 FAILED — entry ", eid, " has ", nrow(roster),
           " rows, expected 15", call. = FALSE)
    }
    cnts <- table(factor(roster$position, levels = c("GK", "DEF", "MID", "FWD")))
    if (!identical(as.integer(cnts), c(2L, 5L, 5L, 3L))) {
      stop(">>> [subs] GATE 5 FAILED — entry ", eid, " positions ",
           paste(names(cnts), cnts, collapse = " "), call. = FALSE)
    }
    if (anyDuplicated(roster$team)) {
      stop(">>> [subs] GATE 5 FAILED — entry ", eid,
           " still holds two players at ",
           paste(unique(roster$team[duplicated(roster$team)]), collapse = ", "),
           call. = FALSE)
    }
    if (anyDuplicated(roster$player_id)) {
      stop(">>> [subs] GATE 5 FAILED — entry ", eid, " duplicate player_id",
           call. = FALSE)
    }
    
    p2_rows[[eid]] <- tibble(
      pick_id    = vapply(seq_len(15L), function(i) uuid::UUIDgenerate(),
                          character(1)),
      entry_id   = eid,
      player_id  = as.integer(roster$player_id),
      slot       = roster$position,
      created_at = now,
      period     = 2L
    )
  }
  p2_rows <- bind_rows(p2_rows)
  message("    [subs] gate 5 OK — ", length(affected),
          " period-2 rosters valid (", nrow(p2_rows), " rows)")
  
  # --- Build transfers rows -------------------------------------------------
  
  tr_rows <- tibble(
    transfer_id   = vapply(seq_len(nrow(subs)), function(i) uuid::UUIDgenerate(),
                           character(1)),
    entry_id      = subs$entry_id,
    out_player_id = subs$out_player_id,
    in_player_id  = subs$in_player_id,
    type          = TYPE,
    effective_gw  = EFFECTIVE_GW,
    note          = paste0(subs$out_name, " moved to ", subs$clash_club,
                           " (", subs$clash_with, " held). Club-clash ",
                           "auto-sub: next listed at former club ",
                           "(price, then name)."),
    created_at    = now_str
  )
  
  # --- Write / review -------------------------------------------------------
  
  if (DRY_RUN) {
    readr::write_csv(p2_rows, REVIEW_PICKS_CSV)
    readr::write_csv(tr_rows, REVIEW_TRANSFERS_CSV)
    message(">>> [subs] DRY RUN complete — nothing written. Would append ",
            nrow(p2_rows), " roster_picks rows (period 2) and ",
            nrow(tr_rows), " transfers rows.\n",
            "    review: ", REVIEW_PICKS_CSV, "\n            ",
            REVIEW_TRANSFERS_CSV)
  } else {
    sheet_append(sheet_id(), p2_rows, sheet = "roster_picks")
    message("    [subs] appended ", nrow(p2_rows), " roster_picks rows")
    sheet_append(sheet_id(), tr_rows, sheet = "transfers")
    message("    [subs] appended ", nrow(tr_rows), " transfers rows")
    
    # Verify by re-read.
    chk_p <- read_sheet(sheet_id(), sheet = "roster_picks", col_types = "c") |>
      mutate(period = suppressWarnings(as.integer(period)))
    chk_t <- read_sheet(sheet_id(), sheet = "transfers", col_types = "c")
    
    p2 <- chk_p |> filter(period == 2L)
    per_entry <- p2 |> count(entry_id)
    ok_p <- nrow(p2) == nrow(p2_rows) &&
      all(per_entry$n == 15L) &&
      setequal(per_entry$entry_id, affected) &&
      sum(chk_p$period == 1L, na.rm = TRUE) == n_p1
    ok_t <- sum(chk_t$transfer_id %in% tr_rows$transfer_id) == nrow(tr_rows)
    
    if (!ok_p || !ok_t) {
      stop(">>> [subs] VERIFICATION FAILED — picks ok=", ok_p,
           " transfers ok=", ok_t, ". Inspect the tabs by hand; rollback = ",
           "delete period-2 rows and the ", nrow(tr_rows),
           " transfer_ids in ", REVIEW_TRANSFERS_CSV, call. = FALSE)
    }
    readr::write_csv(tr_rows, REVIEW_TRANSFERS_CSV)   # keep ids for rollback
    message(">>> [subs] LIVE WRITE complete and verified — ",
            nrow(p2), " period-2 rows across ", nrow(per_entry),
            " entries; period-1 rows unchanged (", n_p1, "); ",
            nrow(tr_rows), " transfers rows. Flip DRY_RUN back to TRUE.")
  }
  
}
