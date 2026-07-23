# run_stats_update.R ----------------------------------------------------------
# One-command daily stats update for the World Cup 2026 Challenge.
#
# Run with (from the PROJECT ROOT, so the relative paths resolve):
#   source("run_stats_update.R")
#
# What it does, in order:
#   1. Clears the stale fixtures cache (data/api_cache/_matches__*.json) so the
#      run reads the LIVE list of finished matches. This is the bug that bit us
#      on 13 Jun: the /matches response was cached and never invalidated, so
#      re-runs read a frozen snapshot and ingested nothing new. Clearing it here
#      is version-agnostic — harmless if you're on the use_cache=FALSE fix,
#      essential if you're not.
#   2. source("map_api_players.R")   — maps any newly-seen squads to column H
#      BEFORE ingestion's owned-player gate checks them.
#   3. source("ingest_match_stats.R") — writes raw stats for every finished
#      match not already in the player_match_stats tab.
#
# There is NO separate scoring step. R/scoring.R is a pure read-time engine and
# the leaderboard reads player_match_stats through the 5-minute cache, so the
# public board reflects this run within ~5 minutes — no redeploy.
#
# OUTPUT: a delimited report prints at the very end. Copy everything between the
# "vvvvv" and "^^^^^" lines and paste it back to Claude — it surfaces versions,
# whether the fixtures fetch was live, and every mismatch / skip / unmapped flag.
#
# Flag legend (what each report flag means):
#   *** MISMATCH ***          -> almost always an own goal. Add
#                                match_id = <n> to OWN_GOAL_ALLOWANCE in
#                                ingest_match_stats.R, re-run, then set own_goals
#                                on the scorer's row in the Sheet (the -5).
#   *** OWNED PLAYER UNMAPPED -> an owned player couldn't auto-map. Hand-map the
#                                api_player_id into column H of the players tab.
#   SKIP - fixture score      }  API hasn't reconciled yet (pure lag). No action;
#   missing / player-stats    }  picked up automatically on the next run.
#   empty                     }
# -----------------------------------------------------------------------------

message(">>> SOURCED run_stats_update.R (v1) at ",
        format(Sys.time(), "%Y-%m-%d %H:%M:%S"))

# --- 0. Version-agnostic safety: clear the stale fixtures snapshot ------------

.fixture_cache <- list.files("data/api_cache", pattern = "^_matches__",
                             full.names = TRUE)
.n_cleared <- length(.fixture_cache)
if (.n_cleared > 0) file.remove(.fixture_cache)
message(">>> [update] cleared ", .n_cleared,
        " stale fixtures cache file(s) before run")

# --- 1+2. Run map then ingest, teeing every message() into a capture buffer ---
# Handlers that return normally do NOT consume the condition, so each message
# still prints live to the console AND lands in .upd_log for the report.

.upd_log <- character(0)
.tee <- function(m) {
  txt   <- sub("\n$", "", conditionMessage(m))
  parts <- strsplit(txt, "\n", fixed = TRUE)[[1]]
  .upd_log <<- c(.upd_log, parts)
}

.run_ok  <- TRUE
.run_err <- NULL

withCallingHandlers(
  tryCatch(
    {
      source("map_api_players.R")
      source("ingest_match_stats.R")
    },
    error = function(e) {
      .run_ok  <<- FALSE
      .run_err <<- conditionMessage(e)
    }
  ),
  message = .tee
)

# --- 3. Build the copy-pasteable report --------------------------------------

.pick <- function(pattern) grep(pattern, .upd_log, value = TRUE, perl = TRUE)

# All the scripts' own signal markers (excludes googlesheets4 ✔ noise etc.)
.run_log <- grep("^>>> ", .upd_log, value = TRUE)

# Version markers
.versions <- .pick("^>>> SOURCED (map_api_players|ingest_match_stats)")

# Proof we hit the wire this run (cleared cache => a real GET /matches)
.live_fetch <- any(grepl("\\[api\\] GET /matches", .upd_log))

# The fixtures verdict line from ingest
.fixtures_line <- .pick("\\[ingest\\] finished:")

# Anything that needs a human decision
.attention <- .pick(paste0(
  "MISMATCH|OWNED PLAYER UNMAPPED|UNMAPPED OWNED:|SKIPPED matches|",
  "\\[teams\\] WARNING|\\[ingest\\] SKIP |HTTP [45][0-9][0-9]"
))

# Map outcome: unmatched / ambiguous rows from the report CSV (paste-ready)
.unmatched_tbl <- NULL
.rep_file <- "data/api_player_mapping_report.csv"
if (file.exists(.rep_file)) {
  .mr <- tryCatch(read.csv(.rep_file, stringsAsFactors = FALSE),
                  error = function(e) NULL)
  if (!is.null(.mr) && nrow(.mr) > 0 && "matched_name" %in% names(.mr)) {
    .um <- .mr[is.na(.mr$matched_name) | .mr$matched_name == "", , drop = FALSE]
    if (nrow(.um) > 0) {
      .keep <- intersect(c("api_player_id", "api_name", "team_name",
                           "api_pos", "method"), names(.um))
      .unmatched_tbl <- .um[, .keep, drop = FALSE]
    }
  }
}

.w <- function(...) cat(..., "\n", sep = "")

cat("\n\n")
.w("vvvvv STATS UPDATE REPORT \u2014 COPY EVERYTHING BELOW THIS LINE vvvvv")
.w("")
.w("Run at:        ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
.w("Result:        ", if (.run_ok) "completed" else paste0("FAILED \u2014 ", .run_err))
.w("Fixtures cache: cleared ", .n_cleared, " stale file(s) before run")
.w("Live fetch:    ", if (.live_fetch) "YES (read live /matches)"
   else "NO \u2014 fixtures may be stale; check working dir is project root")
.w("")

.w("Versions:")
if (length(.versions)) for (ln in .versions) .w("  ", ln) else .w("  (none captured)")
.w("")

.w("Fixtures:")
if (length(.fixtures_line)) for (ln in .fixtures_line) .w("  ", ln) else
  .w("  (no fixtures summary line \u2014 see run log below)")
.w("")

.w("NEEDS ATTENTION:")
if (length(.attention)) {
  for (ln in .attention) .w("  ", ln)
} else {
  .w("  none \u2014 clean run, nothing flagged")
}
.w("")

.w("Mapping (unmatched / ambiguous):")
if (is.null(.unmatched_tbl)) {
  .w("  all API players seen are mapped \u2014 nothing to resolve")
} else {
  .w("  ", nrow(.unmatched_tbl), " to resolve (paste column H by hand if any are owned):")
  for (ln in capture.output(print(.unmatched_tbl, row.names = FALSE))) .w("    ", ln)
}
.w("")

.w("Full run log (script markers):")
if (length(.run_log)) for (ln in .run_log) .w("  ", ln) else
  .w("  (empty \u2014 the run did not start; check the error above)")
.w("")

.w("Legend: MISMATCH=likely own goal (set OWN_GOAL_ALLOWANCE + own_goals cell);")
.w("        OWNED PLAYER UNMAPPED=hand-map column H; SKIP fixture/stats=API lag,")
.w("        auto-retried next run. Leaderboard updates within ~5 min, no redeploy.")
.w("")
.w("^^^^^ STATS UPDATE REPORT \u2014 COPY EVERYTHING ABOVE THIS LINE ^^^^^")
cat("\n")

invisible(NULL)