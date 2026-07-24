# fix_pl_config2.R -------------------------------------------------------
# contest_config rewrite, take 2: lock_at must be a real DATETIME cell
# (the lock countdown calls format() on it expecting POSIXct; a text
# cell dispatches to format.default and errors). Writing POSIXct makes
# googlesheets4 create a datetime cell, and read_sheet returns POSIXct.
#
# Overwrites the tab. Safe to re-run. lock_at remains the PLACEHOLDER
# 2026-08-21 18:00 UTC — when TV picks confirm the real kickoff, edit
# the cell in the browser AS A DATETIME (or re-run this with the value
# changed).
#
#   source("R/sheets.R")
#   source("fix_pl_config2.R")

library(googlesheets4)
library(tibble)

message(">>> fix_pl_config2 — target Sheet: ", sheet_id())

config <- tibble(
  contest_name         = "PL Contest 26/27",
  roster_total         = 15L,
  min_gk  = 2L, max_gk  = 2L,
  min_def = 5L, max_def = 5L,
  min_mid = 5L, max_mid = 5L,
  min_fwd = 3L, max_fwd = 3L,
  max_entries_per_user = 5L,
  entry_fee            = 10L,
  lock_at              = as.POSIXct("2026-08-21 18:00:00", tz = "UTC"),
  gameweek_first       = 1L,
  gameweek_last        = 19L
)

googlesheets4::sheet_write(config, ss = sheet_id(),
                           sheet = "contest_config")
message(">>> fix_pl_config2 complete — lock_at written as datetime: ",
        format(config$lock_at, "%Y-%m-%d %H:%M:%S %Z"))