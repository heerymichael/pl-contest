# Transfer-window helpers.
#
# Mirrors the fresh/cached split in R/lock.R:
#   transfer_window_state()         fresh Sheets read — use in every write path
#   transfer_window_state_cached()  5-min config cache — UI rendering only
#
# Both return one of:
#   "none"    config carries no window (columns absent / blank) — behave as
#             if there is no window at all
#   "before"  now < transfers_open_at
#   "open"    transfers_open_at <= now < transfers_close_at
#   "closed"  now >= transfers_close_at
#
# can_edit_entry() is the single answer to "may this entry's roster be
# changed right now": TRUE pre-lock (normal editing) or while the window
# is open (transfers). The picker keys its edit-mode UI off the cached
# form; the save handler re-checks the fresh form before writing.
#
# Config values arrive as POSIXct in UTC from the Sheet (written by
# add_transfer_config.R). All comparisons are absolute — no session-tz
# dependence. Sheets stores datetimes as fractional days, so a value like
# 08:00 UTC (1/3 day) reads back as 07:59:59.999; every read is rounded
# to the minute here so state flips and display text agree.

message(">>> SOURCED R/transfers.R at ", format(Sys.time()))

library(shiny)
library(lubridate)

.tw_minute <- function(x) round_date(as_datetime(x[1]), unit = "minute")

# LOCAL TESTING ONLY. If PL_FORCE_WINDOW_STATE is set in the R session
# (Sys.setenv(), never .Renviron, never on shinyapps.io) it overrides the
# computed state so the window UI and write path can be exercised before
# Thursday against a sandbox copy of the Sheet (GS_SHEET_ID pointed at
# the copy). Logged loudly on every call so it cannot be forgotten.
.tw_forced_state <- function() {
  f <- Sys.getenv("PL_FORCE_WINDOW_STATE", unset = "")
  if (!nzchar(f)) return(NULL)
  message("!!! transfer window state FORCED to '", f,
          "' by PL_FORCE_WINDOW_STATE — local testing only")
  f
}

.tw_state_from_config <- function(config, now = Sys.time()) {
  forced <- .tw_forced_state()
  if (!is.null(forced)) return(forced)
  open_at  <- config$transfers_open_at  %||% NULL
  close_at <- config$transfers_close_at %||% NULL
  if (is.null(open_at) || is.null(close_at) ||
      length(open_at) == 0 || length(close_at) == 0 ||
      is.na(open_at[1]) || is.na(close_at[1])) {
    return("none")
  }
  open_at  <- .tw_minute(open_at)
  close_at <- .tw_minute(close_at)
  if (now < open_at)  return("before")
  if (now < close_at) return("open")
  "closed"
}

# Fresh read. Use in any write path.
transfer_window_state <- function() {
  config <- get_contest_config()   # uncached
  state  <- .tw_state_from_config(config)
  message(">>> transfer_window_state (fresh) — ", state)
  state
}

# Cached read. UI rendering only. Worst-case staleness 5 minutes; every
# write path calls the fresh form above, so writes are never at risk.
transfer_window_state_cached <- function() {
  config <- get_config_cached()
  state  <- .tw_state_from_config(config)
  message(">>> transfer_window_state (cached) — ", state)
  state
}

# Window parameters, with the same defaults the runbook assumes so a
# config missing a column degrades to the documented rule rather than
# to NA arithmetic.
transfers_max <- function(config = get_config_cached()) {
  as.integer(config$transfers_max %||% 2L)
}

transfer_effective_gw <- function(config = get_config_cached()) {
  as.integer(config$transfer_effective_gw %||% 3L)
}

# The period a window transfer writes into. Derived from the gameweeks
# tab via gameweek_period() (R/gameweeks.R) so the roster model and the
# scoring model can never disagree; falls back to 2 if the tab is
# unreadable, which is the only period a window has ever targeted.
transfer_target_period <- function(config = get_config_cached()) {
  p <- tryCatch(gameweek_period(transfer_effective_gw(config)),
                error = function(e) NA_integer_)
  if (is.na(p)) 2L else as.integer(p)
}

# May an existing entry's roster be changed right now? Cached form for
# UI; callers on the write path must re-check with the fresh forms.
can_edit_entry_cached <- function() {
  !is_locked_cached() || transfer_window_state_cached() == "open"
}

can_edit_entry <- function() {
  !is_locked() || transfer_window_state() == "open"
}

# Human-readable window bounds for banner / rules copy.
transfer_window_text <- function(config = get_config_cached()) {
  open_at  <- config$transfers_open_at  %||% NULL
  close_at <- config$transfers_close_at %||% NULL
  if (is.null(open_at) || is.null(close_at)) return(NULL)
  fmt <- function(x) format(with_tz(.tw_minute(x), "Europe/London"),
                            "%a %d %b, %H:%M")
  paste0(fmt(open_at), " \u2013 ", fmt(close_at))
}