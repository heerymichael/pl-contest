# setup_pl_config.R ------------------------------------------------------
# One-time write of contest rules to the `contest_config` tab.
#
# The lineup creator reads all roster constraints from these keys
# (with code fallbacks), so this is where the 15-player 2/5/5/3
# shape is authoritatively defined. min == max per position gives
# exact counts through the existing min/max validation.
#
# lock_at is a PLACEHOLDER: 2026-08-21 18:00 UTC assumes a 20:00 BST
# kickoff for Arsenal v Coventry. Update the cell in the browser when
# the GW1 TV picks confirm the real kickoff (lock = kickoff - 1h).
#
# Refuses to run if contest_config already has rows.
#
# Run in two steps per the auth pattern:
#   source("R/sheets.R")
#   source("setup_pl_config.R")

library(googlesheets4)
library(tibble)

message(">>> setup_pl_config — target Sheet: ", sheet_id())

config <- tribble(
  ~key,                    ~value,
  "contest_name",          "PL Contest 26/27",        # placeholder — rename pending
  "roster_total",          "15",
  "min_gk",  "2",  "max_gk",  "2",
  "min_def", "5",  "max_def", "5",
  "min_mid", "5",  "max_mid", "5",
  "min_fwd", "3",  "max_fwd", "3",
  "max_entries_per_user",  "5",
  "entry_fee",             "10",
  "lock_at",               "2026-08-21 18:00:00",     # UTC — PLACEHOLDER, see header
  "gameweek_first",        "1",
  "gameweek_last",         "19"
)

existing <- read_sheet(sheet_id(), sheet = "contest_config",
                       col_types = "cc")
if (nrow(existing) > 0) {
  stop(">>> [config] contest_config already has ", nrow(existing),
       " rows — refusing to overwrite. Edit cells in the browser ",
       "instead.", call. = FALSE)
}

googlesheets4::sheet_write(config, ss = sheet_id(),
                           sheet = "contest_config")
message(">>> setup_pl_config complete — wrote ", nrow(config), " keys")