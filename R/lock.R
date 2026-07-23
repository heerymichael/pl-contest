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

# Countdown banner — pre-lock it shows the lock rule (left) and the live
# timer chip (right, driven by the countdown JS in app.R). Post-lock the
# countdown is over, so it becomes a static notice in the same neon band,
# centred, with no timer. With no timer element present the countdown JS
# no-ops (tick() bails when the timer span is absent), so it never repaints
# this state.
countdown_banner_ui <- function(locked = FALSE) {
  if (locked) {
    # Temporary post-lock notice. Update/remove once the payout
    # structure is confirmed.
    return(div(
      id    = "countdown-banner",
      class = "countdown-banner banner-neutral banner-notice",
      tags$span(
        class = "countdown-banner-message",
        "Final payments are being chased — the payout structure will be confirmed once complete, before the end of Matchday 1."
      )
    ))
  }
  div(
    id    = "countdown-banner",
    class = "countdown-banner banner-neutral",
    tags$span(
      class = "countdown-banner-message",
      "Lineups lock 1 hour before first kickoff"
    ),
    tags$span(
      id    = "countdown-banner-timer",
      class = "countdown-banner-timer",
      "—"   # placeholder until JS populates
    )
  )
}