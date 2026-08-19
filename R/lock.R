# Lock-time helpers.
#
# is_locked() does a fresh Sheets read every call — the cached config
# is unsafe near the lock moment (5-min TTL could let stale "unlocked"
# answers through). Called as a defence in every submit/save handler.
#
# Lock state propagates to the UI via the is_locked_reactive in app.R,
# which depends on a lock_tick reactiveVal bumped by the countdown JS
# when it detects zero. That makes the UI flip from picker to locked
# at the moment of lock, even without a page refresh.

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

# Standing announcement of the Gameweek 3 transfer window, shown as a
# quiet second line beneath the live countdown. Deliberately copy-only:
# no dates, no counts, no countdown of its own — the full phase-aware
# treatment (countdown to window open, then to window close) lands with
# the transfer-window config in TRANSFER_WINDOW.md batch 3. Until that
# config exists there is nothing to count down to.
BANNER_TRANSFER_NOTICE <-
  "A short transfer window will open between Gameweeks 2 and 3"

# Post-lock banner copy. Replaces the pre-lock countdown once lock_at
# passes, and carries the transfer-window announcement forward — without
# it, the notice disappears at exactly the moment entrants can no longer
# edit and most want to know what their options are.
BANNER_LOCKED_NOTICE <-
  paste("Contest locked \u2014 rosters are fixed until the transfer window",
        "between Gameweeks 2 and 3")

# Countdown banner — pre-lock it shows the lock rule and the live timer
# chip (driven by the countdown JS in app.R) on one row, with the
# transfer-window notice on a second, quieter row below. Post-lock the
# countdown is over, so it becomes a static notice in the same neon
# band, centred, with no timer. With no timer element present the
# countdown JS no-ops (tick() bails when the timer span is absent), so
# it never repaints this state.
#
# Layout note: the stacked two-row form is selected in CSS via
# :has(.countdown-banner-secondary), NOT via a modifier class on the
# banner. The countdown JS reassigns banner.className wholesale on
# every tick, so any modifier class added here would be wiped one
# second later. :has() keys off a child instead and survives.
countdown_banner_ui <- function(locked = FALSE) {
  if (locked) {
    # Single centred line, no timer. Kept as one span rather than the
    # two-row pre-lock structure so the :has() stacking rule doesn't
    # fire and fight banner-notice's centring. With no timer element
    # present the countdown JS bails before touching className, so this
    # state is never repainted.
    return(div(
      id    = "countdown-banner",
      class = "countdown-banner banner-neutral banner-notice",
      tags$span(
        class = "countdown-banner-message",
        BANNER_LOCKED_NOTICE
      )
    ))
  }
  div(
    id    = "countdown-banner",
    class = "countdown-banner banner-neutral",
    div(
      class = "countdown-banner-row",
      tags$span(
        class = "countdown-banner-message",
        "Lineups lock at the opening kickoff"
      ),
      tags$span(
        id    = "countdown-banner-timer",
        class = "countdown-banner-timer",
        "—"   # placeholder until JS populates
      )
    ),
    tags$span(
      class = "countdown-banner-secondary",
      BANNER_TRANSFER_NOTICE
    )
  )
}