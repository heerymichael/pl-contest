# Lock-time helpers.
#
# is_locked() does a fresh Sheets read every call — the cached config
# is unsafe near the lock moment (5-min TTL could let stale "unlocked"
# answers through). Called as a defence in every submit/save handler.
#
# Lock state propagates to the UI via the is_locked_reactive in app.R,
# which depends on a lock_tick reactiveVal bumped by the countdown JS
# when it detects zero. That makes the UI flip from picker to locked
# at the moment of lock, even without a page refresh. The same tick
# drives the transfer-window state (R/transfers.R), so the banner,
# the editor and the window flip together.

message(">>> SOURCED R/lock.R at ", format(Sys.time()))

library(shiny)
library(lubridate)

# Fresh-read lock check. Use this in any write path. Returns TRUE if
# contest is locked (i.e., current time is at or past lock_at).
is_locked <- function() {
  config <- get_contest_config()   # uncached
  locked <- Sys.time() >= as_datetime(config$lock_at)
  message(">>> is_locked (fresh) — ", locked)
  locked
}

# Cached lock check for UI rendering paths only. Uses the 5-minute TTL
# config cache so first paint isn't blocked behind a live Sheets
# round-trip. Worst-case staleness: an admin edit to lock_at takes up
# to 5 minutes to reach the UI. Every write path still calls the
# fresh-read is_locked() above, so writes are never at risk.
is_locked_cached <- function() {
  config <- get_config_cached()
  locked <- Sys.time() >= as_datetime(config$lock_at)
  message(">>> is_locked (cached) — ", locked)
  locked
}

# Locked replacement for the Build a Lineup view.
locked_view <- function() {
  div(
    class = "view",
    div(
      class = "lineup-header",
      h2("Contest locked")
    ),
    div(
      class = "locked-message",
      p("The contest is locked. No further entries can be created."),
      p("If you've already submitted lineups, you can still view them from My Lineups (once you're logged in).")
    )
  )
}

# Pre-lock second line: standing announcement of the Gameweek 3 window.
BANNER_TRANSFER_NOTICE <-
  "A short transfer window will open between Gameweeks 2 and 3"

# Post-lock, no window configured (or window state unknown).
BANNER_LOCKED_NOTICE <-
  paste("Contest locked \u2014 rosters are fixed until the transfer window",
        "between Gameweeks 2 and 3")

# Post-window static notice.
BANNER_WINDOW_CLOSED_NOTICE <-
  "Transfer window closed \u2014 rosters are fixed for Gameweeks 3\u201319"

# ISO-8601 UTC string for the countdown JS, or NULL.
.banner_iso <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x[1])) return(NULL)
  format(round_date(as_datetime(x[1]), unit = "minute"),
         "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

# Countdown banner. Phase-aware:
#
#   pre-lock            lock rule + live timer to lock_at, transfer-window
#                       notice as a quieter second row
#   locked / before     "Contest locked" + timer to transfers_open_at
#   locked / open       magenta band, timer to transfers_close_at
#   locked / closed     static notice, no timer
#   locked / none       static notice (no window in config)
#
# The target datetime travels on the element as data-until (ISO, UTC);
# the JS in app.R reads it off the element on every tick, so each
# re-render can point at a new target without a custom message. A
# data-band attribute pins the colour band (the JS otherwise picks
# neutral/warn/urgent from the seconds remaining). When there is no
# timer element the JS no-ops, so static states are never repainted.
#
# Layout note: the stacked two-row form is selected in CSS via
# :has(.countdown-banner-secondary), NOT via a modifier class on the
# banner, because the countdown JS reassigns banner.className on every
# tick.
countdown_banner_ui <- function(locked = FALSE, window_state = "none",
                                config = NULL) {
  if (is.null(config)) config <- get_config_cached()
  
  static_notice <- function(text) {
    div(
      id    = "countdown-banner",
      class = "countdown-banner banner-neutral banner-notice",
      tags$span(class = "countdown-banner-message", text)
    )
  }
  
  timed <- function(message_text, until_iso, band = NULL,
                    secondary = NULL, zero_text = NULL) {
    div(
      id           = "countdown-banner",
      class        = paste("countdown-banner", band %||% "banner-neutral"),
      `data-until` = until_iso,
      `data-band`  = band,
      `data-zero`  = zero_text,
      div(
        class = "countdown-banner-row",
        tags$span(class = "countdown-banner-message", message_text),
        tags$span(id = "countdown-banner-timer",
                  class = "countdown-banner-timer", "\u2014")
      ),
      if (!is.null(secondary))
        tags$span(class = "countdown-banner-secondary", secondary)
    )
  }
  
  if (!locked) {
    return(timed("Lineups lock at the opening kickoff",
                 .banner_iso(config$lock_at),
                 secondary = BANNER_TRANSFER_NOTICE,
                 zero_text = "Contest is locked"))
  }
  
  switch(window_state,
         "before" = timed("Contest locked \u2014 transfer window opens in",
                          .banner_iso(config$transfers_open_at),
                          zero_text = "Transfer window is open"),
         "open"   = timed("Transfer window open \u2014 closes in",
                          .banner_iso(config$transfers_close_at),
                          band = "banner-window",
                          secondary = paste0("Up to ",
                                             as.integer(config$transfers_max %||% 2L),
                                             " changes per entry, from My Lineups. ",
                                             "Your roster at the close is what counts."),
                          zero_text = "Transfer window is closed"),
         "closed" = static_notice(BANNER_WINDOW_CLOSED_NOTICE),
         static_notice(BANNER_LOCKED_NOTICE)
  )
}