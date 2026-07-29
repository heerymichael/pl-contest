# map_players_2526.R ------------------------------------------------------
# One-off: seed api_player_id on the players tab from the 25/26 scores
# data. TheStatsAPI player ids (pl_xxxxxxxx) are the durable bridge
# between the contest pool and the stats pipeline — the same role as
# column H in the WC contest, but populated up front for the whole pool
# rather than reactively as the ingest gate trips.
#
# Sources, in order of authority:
#   1. data/api_id_overrides.csv  — hand-confirmed identity fixes
#      (nickname / short-form / bare-name cases the matcher must not
#      guess). Overrides always win.
#   2. data/pl2526_scores.rds     — auto-match by normalised full name.
#
# Safety rails:
#   - DRY_RUN is TRUE by default: prints the summary, writes the full
#     proposed mapping to /tmp/map_players_2526_review.csv, and touches
#     nothing. Review, then EDIT THE FLAG BELOW IN THIS FILE and re-run.
#     (Console assignment of DRY_RUN is overwritten on source() — the
#     OWN_GOAL_ALLOWANCE lesson.)
#   - Never overwrites a non-blank api_player_id already on the tab;
#     conflicts are reported, not resolved.
#   - Refuses ambiguous auto-matches (one normalised name resolving to
#     more than one scores player).
#   - Refuses to write if any api id would be assigned to two pool
#     players.
#   - Hard-verifies its own write by re-reading the tab fresh.
#
# Usage:
#   source("R/sheets.R")            # let auth complete first
#   source("map_players_2526.R")    # dry run — review the output
#   # then edit DRY_RUN below to FALSE and source again
# --------------------------------------------------------------------------

DRY_RUN <- FALSE

library(dplyr)
library(googlesheets4)

message(">>> map_players_2526 — target Sheet: ", sheet_id(),
        if (DRY_RUN) "  [DRY RUN]" else "  [LIVE WRITE]")

stopifnot(file.exists("data/pl2526_scores.rds"),
          file.exists("data/api_id_overrides.csv"))

# --- normaliser -----------------------------------------------------------
# chartr first: iconv ASCII//TRANSLIT drops stroked/slashed letters
# (Đ, Ł, Ø) on some platforms instead of converting them — the Petrović
# gap. Everything else (accents, case, spacing, punctuation) falls to
# iconv + the gsub.
norm_name <- function(x) {
  x <- chartr("\u0110\u0111\u0141\u0142\u00D8\u00F8", "DdLlOo", x)
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  x[is.na(x)] <- ""
  gsub("[^a-z]", "", tolower(x))
}

# --- read inputs ----------------------------------------------------------
players <- read_sheet(sheet_id(), sheet = "players", col_types = "c")
message("    [map] players tab — ", nrow(players), " rows, ",
        ncol(players), " cols")
stopifnot("api_player_id" %in% names(players),
          "player_id" %in% names(players))

sc <- readRDS("data/pl2526_scores.rds")
sc$nn <- norm_name(sc$player_name)

# Ambiguity guard: any normalised name shared by two scores players is
# excluded from auto-matching entirely (overrides can still reach them).
amb <- unique(sc$nn[duplicated(sc$nn)])
if (length(amb) > 0) {
  message("!!  [map] ", length(amb),
          " ambiguous normalised name(s) excluded from auto-match:")
  for (a in amb) message("     - ",
    paste(sc$player_name[sc$nn == a], collapse = " / "))
}
lookup <- sc |>
  filter(!nn %in% amb) |>
  transmute(nn,
            auto_api_id   = player_id,
            auto_api_name = player_name,
            auto_club     = club_code,
            auto_pos      = position)

# --- auto-match by normalised name ---------------------------------------
pool <- players |>
  mutate(nn = norm_name(name)) |>
  left_join(lookup, by = "nn")

# --- overrides (always win) ----------------------------------------------
ov <- read.csv("data/api_id_overrides.csv", colClasses = "character") |>
  transmute(player_id, ov_api_id = api_player_id)

bad_ov <- setdiff(ov$ov_api_id, sc$player_id)
if (length(bad_ov) > 0) {
  stop("Override api id(s) not present in 25/26 scores data: ",
       paste(bad_ov, collapse = ", "))
}
bad_pid <- setdiff(ov$player_id, pool$player_id)
if (length(bad_pid) > 0) {
  stop("Override player_id(s) not present on the players tab: ",
       paste(bad_pid, collapse = ", "))
}

sc_ref <- sc |> transmute(api_id = player_id, ov_api_name = player_name,
                          ov_club = club_code, ov_pos = position)

pool <- pool |>
  left_join(ov, by = "player_id") |>
  left_join(sc_ref, by = c("ov_api_id" = "api_id")) |>
  mutate(
    existing    = ifelse(is.na(api_player_id) | api_player_id == "",
                         NA_character_, api_player_id),
    proposed    = coalesce(ov_api_id, auto_api_id),
    matched_as  = coalesce(ov_api_name, auto_api_name),
    match_club  = coalesce(ov_club, auto_club),
    match_pos   = coalesce(ov_pos, auto_pos),
    source      = case_when(!is.na(ov_api_id)   ~ "override",
                            !is.na(auto_api_id) ~ "name",
                            TRUE                ~ "unmatched"),
    # Never overwrite a non-blank cell; keep it and flag disagreement.
    final       = coalesce(existing, proposed),
    conflict    = !is.na(existing) & !is.na(proposed) &
                  existing != proposed
  )

# --- sanity: no api id assigned twice ------------------------------------
dup_ids <- pool |>
  filter(!is.na(final)) |>
  count(final) |>
  filter(n > 1)
if (nrow(dup_ids) > 0) {
  print(pool |>
    filter(final %in% dup_ids$final) |>
    select(player_id, name, team, position, final, source))
  stop("Same api id assigned to multiple pool players — refusing to write.")
}

# --- club/position disagreement: REPORT ONLY -----------------------------
# Disagreement is a weak signal here, not grounds for dropping:
# FanTeam classes many wingers MID where the API says FWD (systemic),
# and club differences are mostly genuine summer transfers (26/27 pool
# vs 25/26 season). True same-name collisions are already excluded by
# the ambiguity guard above. Flagged rows land in the review CSV for
# an eyeball pass; the 26/27 ingest gate re-verifies every id against
# live squads in August regardless.
pool <- pool |>
  mutate(flag_review = source == "name" &
           (team != match_club | position != match_pos))
n_flag <- sum(pool$flag_review, na.rm = TRUE)
if (n_flag > 0) {
  message("    [map] ", n_flag, " auto-match(es) with club/position ",
          "disagreement (transfers / winger reclassification) — flagged ",
          "in review CSV, none dropped")
}

# --- report ---------------------------------------------------------------
n_total    <- nrow(pool)
n_override <- sum(pool$source == "override")
n_name     <- sum(pool$source == "name")
n_unm      <- sum(is.na(pool$final))
n_conflict <- sum(pool$conflict)

message("    [map] ", n_total, " pool players: ",
        n_name, " auto-matched by name, ",
        n_override, " via overrides, ",
        n_unm, " unmatched (expected: promoted-club players / summer arrivals)")
if (n_conflict > 0)
  message("!!  [map] ", n_conflict,
          " existing non-blank cell(s) disagree with proposal — kept as-is")

review <- pool |>
  select(player_id, name, team, position, existing,
         final, matched_as, match_club, match_pos, source,
         flag_review, conflict)
write.csv(review, "/tmp/map_players_2526_review.csv", row.names = FALSE)
message("    [map] full mapping table -> /tmp/map_players_2526_review.csv")

message("    [map] override rows applied:")
print(review |> filter(source == "override") |>
        select(player_id, name, team, final, matched_as))

# --- write ----------------------------------------------------------------
if (DRY_RUN) {
  message(">>> DRY RUN complete — nothing written. Review the table, then")
  message(">>> edit DRY_RUN <- FALSE at the top of THIS FILE and re-source.")
} else {
  col_idx <- which(names(players) == "api_player_id")
  stopifnot(length(col_idx) == 1, col_idx <= 26)
  col_letter <- LETTERS[col_idx]
  out <- tibble(api_player_id = ifelse(is.na(pool$final), "", pool$final))
  message(">>> [map] writing api_player_id (", sum(out$api_player_id != ""),
          " ids) to players tab column ", col_letter)
  range_write(sheet_id(), data = out, sheet = "players",
              range = paste0(col_letter, "1"), col_names = TRUE)

  # --- hard verification --------------------------------------------------
  chk <- read_sheet(sheet_id(), sheet = "players", col_types = "c")
  n_chk <- sum(!is.na(chk$api_player_id) & chk$api_player_id != "")
  n_exp <- sum(out$api_player_id != "")
  gab <- chk$api_player_id[chk$player_id == "47"]
  ok_count <- n_chk == n_exp
  ok_gab   <- length(gab) == 1 && identical(gab, "pl_96242662")
  ok_rows  <- nrow(chk) == nrow(players)
  message("    [verify] populated ids: ", n_chk, " (expected ", n_exp, ") ",
          if (ok_count) "OK" else "FAIL")
  message("    [verify] Gabriel (player_id 47) -> ",
          if (length(gab)) gab else "<missing>", " ",
          if (ok_gab) "OK" else "FAIL")
  message("    [verify] row count unchanged: ", nrow(chk), " ",
          if (ok_rows) "OK" else "FAIL")
  if (!(ok_count && ok_gab && ok_rows))
    stop("VERIFICATION FAILED — inspect the players tab before proceeding.")
  message(">>> map_players_2526 complete and verified. Next:")
  message(">>>   source(\"snapshot_pl_reference.R\")   # v4, id-based join")
}
