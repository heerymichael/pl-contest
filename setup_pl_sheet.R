# setup_pl_sheet.R -------------------------------------------------------
# One-time initialisation of the pl-contest-data Google Sheet.
#
# BEFORE RUNNING:
#   1. Create a blank Sheet named `pl-contest-data` in your own Drive
#      (owner = you, same pattern as wc-contest-data).
#   2. Share it with the service-account email as Editor.
#   3. Put its ID in the pl-contest .Renviron as GS_SHEET_ID and
#      restart the R session.
#
# Safe to re-run: existing tabs are never touched; only missing tabs
# are created (header row only, no data). The default `Sheet1` is
# removed once the contest tabs exist.
#
# Deliberately NOT created here: players, teams, matches,
# player_match_stats. Those tabs are created by their ingest scripts
# in the data phase, so their schemas live next to the code that
# writes them rather than being duplicated here.
#
# Run in two steps per the read_sheet auth pattern:
#   source("R/sheets.R")          # step 1 — let auth complete
#   source("setup_pl_sheet.R")    # step 2

library(googlesheets4)
library(tibble)

message(">>> setup_pl_sheet — target Sheet: ", sheet_id())

.tabs <- list(

  users = tibble(
    email         = character(0),
    display_name  = character(0),
    password_hash = character(0),
    is_admin      = integer(0),
    created_at    = character(0)
  ),

  entries = tibble(
    entry_id     = character(0),
    user_email   = character(0),
    entry_name   = character(0),
    created_at   = character(0),
    updated_at   = character(0),
    locked_at    = character(0),
    display_name = character(0)
  ),

  roster_picks = tibble(
    pick_id    = character(0),
    entry_id   = character(0),
    player_id  = character(0),
    slot       = character(0),
    created_at = character(0),
    period     = integer(0)   # defaults to 1 in app writes; future windows
  ),

  contest_config = tibble(
    key   = character(0),
    value = character(0)
  ),

  audit_log = tibble(
    timestamp  = character(0),
    user_email = character(0),
    action     = character(0),
    details    = character(0)
  )
)

existing <- googlesheets4::sheet_names(sheet_id())

for (tab in names(.tabs)) {
  if (tab %in% existing) {
    message("    [setup] `", tab, "` already exists — skipped")
  } else {
    googlesheets4::sheet_write(.tabs[[tab]], ss = sheet_id(), sheet = tab)
    message("    [setup] created `", tab, "`")
  }
}

# Drop the default Sheet1 once real tabs exist
existing <- googlesheets4::sheet_names(sheet_id())
if ("Sheet1" %in% existing && length(existing) > 1) {
  googlesheets4::sheet_delete(sheet_id(), "Sheet1")
  message("    [setup] removed default `Sheet1`")
}

message(">>> setup_pl_sheet complete. Tabs now: ",
        paste(googlesheets4::sheet_names(sheet_id()), collapse = ", "))
