# cleanup_wc_legacy.R — remove WC-contest legacy files from pl-contest
#
# Run from the pl-contest project root:
#   source("cleanup_wc_legacy.R")
#
# Safety: DRY_RUN = TRUE by default — it only reports what it WOULD delete.
# Review the output, then set DRY_RUN <- FALSE *inside this file* and
# re-source to actually delete (same rule as OWN_GOAL_ALLOWANCE: edit the
# file, don't assign at the console — sourcing overwrites console values).
#
# IN SCOPE (deleted):
#   www/flags/                (48 WC country flags — replaced by R/badges.R)
#   www/headshots/            (91 WC-player_id PNGs — wrong ID space for PL)
#   www/wc26-banner.webp      (unreferenced)
#   www/wc26-logo.png         (unreferenced)
#   ingest_match_stats.R      (hard-coded comp_6107 = WC)
#   map_api_players.R         (hard-coded comp_6107 = WC)
#   watch_player_stats.R      (hard-coded comp_6107 = WC)
#   run_stats_update.R        (WC pipeline companion)
#   validate_player_stats.R   (WC pipeline companion)
#   map_owned_overrides.R     (WC pipeline companion)
#   STATS_UPDATE.md           (WC ops runbook)
#   probe_statsapi_pl.R       (superseded by probe_pl_2627.R)
#   R/landing.R               (dead code — not sourced by app.R)
#
# OUT OF SCOPE (deliberately untouched — separate follow-up steps):
#   README.md, GUIDELINES.md      (need rewriting, not deleting)
#   .Renviron                     (USERS_DB_PATH removal is a manual edit)
#   R/leaderboard.R line ~249     (user-visible "World Cup 2026 Challenge"
#                                  eyebrow — code edit, not a deletion)

DRY_RUN <- FALSE

# ---- guard: confirm we're in the right project ------------------------

stopifnot(
  "Run this from the pl-contest project root (pl-contest.Rproj not found)" =
    file.exists("pl-contest.Rproj"),
  "R/badges.R not found — this doesn't look like pl-contest" =
    file.exists("R/badges.R")
)

# ---- targets ----------------------------------------------------------

target_dirs <- c(
  "www/flags",
  "www/headshots"
)

target_files <- c(
  "www/wc26-banner.webp",
  "www/wc26-logo.png",
  "ingest_match_stats.R",
  "map_api_players.R",
  "watch_player_stats.R",
  "run_stats_update.R",
  "validate_player_stats.R",
  "map_owned_overrides.R",
  "STATS_UPDATE.md",
  "probe_statsapi_pl.R",
  "R/landing.R"
)

# ---- pre-flight report ------------------------------------------------

cat("=== Pre-flight ===\n")
for (d in target_dirs) {
  if (dir.exists(d)) {
    n <- length(list.files(d, recursive = TRUE))
    cat(sprintf("  DIR  %-28s exists (%d files)\n", d, n))
  } else {
    cat(sprintf("  DIR  %-28s already absent\n", d))
  }
}
for (f in target_files) {
  cat(sprintf("  FILE %-28s %s\n", f,
              if (file.exists(f)) "exists" else "already absent"))
}

if (DRY_RUN) {
  cat("\nDRY_RUN = TRUE — nothing deleted.\n")
  cat("Edit this file, set DRY_RUN <- FALSE, and re-source to apply.\n")
} else {
  
  # ---- delete ---------------------------------------------------------
  
  cat("\n=== Deleting ===\n")
  for (d in target_dirs) {
    if (dir.exists(d)) {
      unlink(d, recursive = TRUE)
      cat("  removed dir ", d, "\n")
    }
  }
  for (f in target_files) {
    if (file.exists(f)) {
      file.remove(f)
      cat("  removed file", f, "\n")
    }
  }
  
  # ---- hard verification ---------------------------------------------
  
  cat("\n=== Verification ===\n")
  failures <- character(0)
  
  # 1. Every target is gone
  for (p in c(target_dirs, target_files)) {
    if (file.exists(p) || dir.exists(p)) {
      failures <- c(failures, paste0("still present: ", p))
    }
  }
  
  # 2. Every surviving R source file still parses
  live_r <- c("app.R", list.files("R", pattern = "\\.R$", full.names = TRUE))
  for (f in live_r) {
    ok <- tryCatch({ parse(f); TRUE }, error = function(e) FALSE)
    if (!ok) failures <- c(failures, paste0("parse error: ", f))
  }
  
  # 3. No live code references the deleted assets
  #    (headshots/ in leaderboard.R is intentionally allowed — that path
  #     will be repopulated with PL headshots later.)
  ref_check <- function(pattern, label) {
    hits <- character(0)
    for (f in live_r) {
      lines <- readLines(f, warn = FALSE)
      idx <- grep(pattern, lines)
      if (length(idx)) hits <- c(hits, paste0(f, ":", idx))
    }
    if (length(hits)) {
      failures <<- c(failures, paste0(label, " still referenced: ",
                                      paste(hits, collapse = ", ")))
    }
  }
  ref_check('source\\("R/landing\\.R"\\)', "R/landing.R")
  ref_check("landing_ui\\(",              "landing_ui()")
  ref_check('"flags/',                    "www/flags")
  ref_check("wc26-banner|wc26-logo",      "wc26 assets")
  
  if (length(failures)) {
    cat("  FAILED:\n")
    for (msg in failures) cat("    - ", msg, "\n")
    stop("Cleanup verification failed — see above.")
  }
  
  cat("  all targets removed\n")
  cat("  all", length(live_r), "surviving R files parse cleanly\n")
  cat("  no live references to deleted assets\n")
  cat("\nDone. Review with `git status`, then commit.\n")
}