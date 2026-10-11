# add_transfer_config.R -------------------------------------------------------
# One-off: add the Gameweek 3 transfer-window settings to `contest_config`.
#
# Adds four columns to the single config row, leaving every existing value
# (including lock_at) exactly as it is:
#
#   transfers_open_at      datetime   Thu 3 Sep 2026 09:00 Europe/London
#   transfers_close_at     datetime   Fri 4 Sep 2026 19:00 Europe/London
#                                      (1h before the first GW3 kickoff,
#                                      per the standing lock rule)
#   transfers_max          integer    2   (per entry)
#   transfer_effective_gw  integer    3
#
# Datetimes are written as POSIXct so googlesheets4 creates DATETIME cells
# and read_sheet() returns POSIXct — the same lesson as fix_pl_config2.R.
# The Sheet stores them in UTC (lock_at reads back as 19:00 UTC = 20:00
# BST kickoff), so the London times are converted here, not in the app.
#
# The tab is a single wide row, so this is a read-modify-write via
# sheet_write() — the one place that pattern is acceptable (no history to
# lose). Idempotent: re-running overwrites the four columns with the same
# values.
#
# Safety: DRY_RUN TRUE prints the proposed row and writes nothing. Flip
# with set_dry_run("add_transfer_config.R", FALSE); flip back after.
#
# Usage:
#   source("R/sheets.R")                  # auth, selection 2
#   source("add_transfer_config.R")       # dry run
#   set_dry_run("add_transfer_config.R", FALSE); source(...); set_dry_run(..., TRUE)
# ---------------------------------------------------------------------------

DRY_RUN <- TRUE

TRANSFERS_OPEN_AT     <- as.POSIXct("2026-09-03 09:00:00", tz = "Europe/London")
TRANSFERS_CLOSE_AT    <- as.POSIXct("2026-09-04 19:00:00", tz = "Europe/London")
TRANSFERS_MAX         <- 2L
TRANSFER_EFFECTIVE_GW <- 3L

suppressPackageStartupMessages({
  library(googlesheets4)
  library(dplyr)
  library(lubridate)
})

message(">>> add_transfer_config — target Sheet: ", sheet_id(),
        if (DRY_RUN) "  [DRY RUN]" else "  [LIVE WRITE]")

if (!exists("sheet_id")) {
  stop("source(\"R/sheets.R\") first: sheet_id() and auth are required.",
       call. = FALSE)
}

# --- Read ------------------------------------------------------------------

cfg <- read_sheet(sheet_id(), sheet = "contest_config")
if (nrow(cfg) != 1L) {
  stop(">>> [config] expected exactly 1 config row, found ", nrow(cfg),
       call. = FALSE)
}
if (!inherits(cfg$lock_at, "POSIXct")) {
  stop(">>> [config] lock_at did not read back as POSIXct (", class(cfg$lock_at)[1],
       ") — fix that cell before adding more datetimes", call. = FALSE)
}
message("    [config] read 1 row, ", ncol(cfg), " columns; lock_at = ",
        format(with_tz(cfg$lock_at, "Europe/London"), "%a %d %b %H:%M %Z"))

already <- intersect(c("transfers_open_at", "transfers_close_at",
                       "transfers_max", "transfer_effective_gw"), names(cfg))
if (length(already) > 0) {
  message("    [config] column(s) already present, will be overwritten: ",
          paste(already, collapse = ", "))
}

# --- Build -----------------------------------------------------------------

stopifnot(TRANSFERS_CLOSE_AT > TRANSFERS_OPEN_AT,
          TRANSFERS_OPEN_AT > cfg$lock_at)

new_cfg <- cfg |>
  mutate(
    transfers_open_at     = with_tz(TRANSFERS_OPEN_AT,  "UTC"),
    transfers_close_at    = with_tz(TRANSFERS_CLOSE_AT, "UTC"),
    transfers_max         = TRANSFERS_MAX,
    transfer_effective_gw = TRANSFER_EFFECTIVE_GW
  )

message("    [config] proposed:")
message("      transfers_open_at     = ",
        format(TRANSFERS_OPEN_AT,  "%a %d %b %Y %H:%M %Z"), "  (",
        format(new_cfg$transfers_open_at,  "%H:%M UTC"), ")")
message("      transfers_close_at    = ",
        format(TRANSFERS_CLOSE_AT, "%a %d %b %Y %H:%M %Z"), "  (",
        format(new_cfg$transfers_close_at, "%H:%M UTC"), ")")
message("      transfers_max         = ", TRANSFERS_MAX)
message("      transfer_effective_gw = ", TRANSFER_EFFECTIVE_GW)
message("      window length         = ",
        format(round(as.numeric(difftime(TRANSFERS_CLOSE_AT,
                                         TRANSFERS_OPEN_AT, units = "hours")), 1)),
        " hours")

# --- Write / review --------------------------------------------------------

if (DRY_RUN) {
  message(">>> [config] DRY RUN complete — nothing written. ",
          "Flip DRY_RUN to FALSE and re-run to commit.")
} else {
  sheet_write(new_cfg, ss = sheet_id(), sheet = "contest_config")
  message("    [config] wrote contest_config (", ncol(new_cfg), " columns)")
  
  chk <- read_sheet(sheet_id(), sheet = "contest_config")
  ok <- nrow(chk) == 1L &&
    inherits(chk$lock_at, "POSIXct") &&
    inherits(chk$transfers_open_at, "POSIXct") &&
    inherits(chk$transfers_close_at, "POSIXct") &&
    abs(as.numeric(chk$lock_at) - as.numeric(cfg$lock_at)) < 1 &&
    abs(as.numeric(chk$transfers_open_at)  - as.numeric(TRANSFERS_OPEN_AT))  < 1 &&
    abs(as.numeric(chk$transfers_close_at) - as.numeric(TRANSFERS_CLOSE_AT)) < 1 &&
    as.integer(chk$transfers_max) == TRANSFERS_MAX &&
    as.integer(chk$transfer_effective_gw) == TRANSFER_EFFECTIVE_GW
  if (!ok) {
    stop(">>> [config] VERIFICATION FAILED — re-read does not match. ",
         "Inspect contest_config by hand.", call. = FALSE)
  }
  message(">>> [config] LIVE WRITE complete and verified — lock_at unchanged, ",
          "window ", format(with_tz(chk$transfers_open_at, "Europe/London"),
                            "%a %H:%M"),
          " -> ", format(with_tz(chk$transfers_close_at, "Europe/London"),
                         "%a %H:%M %Z"),
          ". Flip DRY_RUN back to TRUE. The app's 5-min config cache picks ",
          "this up without a redeploy.")
}
