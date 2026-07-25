# Top-level navigation header and shared placeholder views.
# Header switches between logged-out and logged-in states.
# Lineup creator view → R/lineup_creator.R
# My Lineups view     → R/my_lineups.R

message(">>> SOURCED R/nav.R at ", format(Sys.time()))

library(shiny)

# Navigation header — shown at the top of every page (auth- and lock-aware).
# `current_username` is NULL when logged out, the username string when logged in.
# `locked` hides the Build a Lineup link once the contest has locked.
# The Leaderboard link is shown to everyone (logged in or not) — roster
# privacy is enforced inside the leaderboard view itself, not here.
nav_header <- function(current_username, current_view = NULL, locked = FALSE) {
  is_authed <- !is.null(current_username)
  
  # Helper: build an actionLink and tag it with nav-link-active if its
  # view_name matches the currently-selected view. tagAppendAttributes
  # is used so Shiny's built-in "action-button" class is preserved
  # (passing class= directly to actionLink would clobber it).
  link <- function(input_id, label, view_name) {
    l <- actionLink(input_id, label)
    if (!is.null(current_view) && current_view == view_name) {
      l <- tagAppendAttributes(l, class = "nav-link-active")
    }
    l
  }
  
  div(
    class = "nav-bar",
    div(
      class = "nav-bar-brand",
      tags$strong(
        tags$span(class = "gaffer-roundel", "XV"),
        tags$span(
          tags$span(class = "gaffer-wordmark", "GAFFER"),
          tags$span(class = "gaffer-sublabel", "FANTASY XV")
        )
      ),
      tags$button(
        id    = "nav-hamburger",
        class = "nav-hamburger",
        type  = "button",
        `aria-label` = "Toggle menu",
        HTML("&#9776;")  # ☰
      )
    ),
    div(
      class = "nav-bar-menu",
      div(
        class = "nav-bar-left",
        if (!locked)   link("nav_lineup",      "Build a Lineup", "lineup"),
        if (is_authed) link("nav_lineups",     "My Lineups",     "lineups"),
        link("nav_leaderboard", "Leaderboard", "leaderboard"),
        link("nav_rules",       "Rules",       "rules"),
        link("nav_scores",      "25/26 Scores", "scores")
      ),
      div(
        class = "nav-bar-right",
        if (is_authed) {
          tagList(
            span(paste("Logged in as", current_username)),
            actionButton("header_change_password", "Change password",
                         class = "btn btn-default btn-sm"),
            actionButton("logout", "Log out", class = "btn btn-default btn-sm")
          )
        } else {
          tagList(
            actionButton("header_login",  "Log in",  class = "btn btn-default btn-sm"),
            actionButton("header_signup", "Sign up", class = "btn btn-primary btn-sm")
          )
        }
      )
    )
  )
}

# Leaderboard view now lives in R/leaderboard.R.

# --- Rules page helpers -------------------------------------------------
# Small view builders for the styled rules page. All styling lives in
# www/styles.css under the "Rules page" section.

# One row in a scoring table: label left, points right.
# neg = TRUE renders the points in coral (negative scores).
rules_score_row <- function(label, pts, neg = FALSE) {
  div(
    class = "rules-score-row",
    span(class = "rules-score-name", label),
    span(class = paste("rules-score-pts", if (neg) "rules-neg"), pts)
  )
}

# Definition-style item: bold lead-in term followed by body text.
# `...` is passed through so callers can mix strings and inline tags
# (e.g. a coral-highlighted span).
rules_item <- function(term, ...) {
  div(
    class = "rules-item",
    span(class = "rules-term", term),
    ...
  )
}

# Roster chip for the dark panel: lime position label + value.
rules_chip <- function(position, value) {
  span(
    class = "rules-chip",
    span(class = "rules-chip-pos", position),
    span(class = "rules-chip-value", value)
  )
}

# Cream pill: plain label, with an optional purple-accented value and an
# optional extra class (rules-pill-muted / rules-pill-final).
rules_pill <- function(label, value = NULL, class = NULL) {
  span(
    class = paste("rules-pill", class),
    label,
    if (!is.null(value)) span(class = "rules-pill-accent", value)
  )
}

# Rules — static content page
rules_view <- function() {
  div(
    class = "view view-narrow",
    
    div(
      class = "lineup-header",
      div(
        div(class = "lineup-eyebrow", "How it works"),
        h2("Contest Rules")
      )
    ),
    
    p(class = "rules-intro",
      "Build a squad of 15 players from across the 20 Premier League clubs
       and accumulate points from their real-life performances over
       Gameweeks 1–19 of the 2026/27 season. There are no weekly lineups to
       set — your best XI is picked automatically every gameweek."),
    
    # Roster construction --------------------------------------------
    div(
      class = "panel-card panel-card-dark rules-section",
      div(class = "rules-section-label", "Roster construction"),
      div(
        class = "rules-chip-row",
        rules_chip("GK",  "exactly 2"),
        rules_chip("DEF", "exactly 5"),
        rules_chip("MID", "exactly 5"),
        rules_chip("FWD", "exactly 3")
      ),
      div(
        class = "rules-pill-row",
        span(class = "rules-meta-pill", "15 players total"),
        span(class = "rules-meta-pill", "Max 1 per club"),
        span(class = "rules-meta-pill", "Up to 5 entries")
      )
    ),
    
    # Bestball ---------------------------------------------------------
    div(
      class = "panel-card rules-section",
      div(class = "rules-section-label", "Bestball — your XI picks itself"),
      p(class = "rules-body",
        "Each gameweek the app automatically selects your highest-scoring
         XI from your 15, using any valid formation:"),
      div(
        class = "rules-pill-row",
        rules_pill("1 GK"),
        rules_pill("3–5 DEF"),
        rules_pill("2–5 MID"),
        rules_pill("1–3 FWD")
      ),
      p(class = "rules-body",
        "Your entry's total is the sum of its optimal-XI points across all
         19 gameweeks. There is nothing to submit weekly, no captains, no
         transfers, and no benching decisions — the best XI is always
         chosen for you."),
      rules_item("Double gameweeks.",
                 "If a player has more than one fixture in a gameweek, their points
         for that gameweek are the sum across all of their fixtures, banked
         before the XI is selected."),
      rules_item("Non-appearances.",
                 "Players who don't feature score 0 for that gameweek. They only
         appear in your XI if the formation minima force it.")
    ),
    
    # Lock time -------------------------------------------------------
    div(
      class = "panel-card rules-section",
      div(class = "rules-section-label", "Lock time"),
      span(class = "rules-lock-chip", "Locks at kickoff of the opening fixture"),
      p(class = "rules-body",
        "Entries lock at kickoff of the opening fixture of the 2026/27
         season (Arsenal v Coventry City, 21 August 2026). After lock, no
         further edits are possible — rosters are fixed for all 19
         gameweeks.")
    ),
    
    # Scoring ----------------------------------------------------------
    div(
      class = "panel-card rules-section",
      div(class = "rules-section-label", "Scoring — outfield players"),
      div(
        class = "rules-score-grid",
        div(
          class = "rules-score-col",
          rules_score_row("Goal",           "+10"),
          rules_score_row("Assist",         "+6"),
          rules_score_row("Shot",           "+1"),
          rules_score_row("Shot on target", "+1"),
          rules_score_row("Tackle",         "+1"),
          rules_score_row("Interception",   "+1")
        ),
        div(
          class = "rules-score-col",
          rules_score_row("Clean sheet (DEF)", "+8"),
          rules_score_row("Clean sheet (MID)", "+4"),
          rules_score_row("Win (DEF)",  "+2"),
          rules_score_row("Draw (DEF)", "+1"),
          rules_score_row("Yellow card", "−1.5", neg = TRUE),
          rules_score_row("Red card",    "−5",   neg = TRUE)
        )
      ),
      div(class = "rules-section-label", "Scoring — goalkeepers"),
      div(
        class = "rules-score-grid",
        div(
          class = "rules-score-col",
          rules_score_row("Save",        "+2"),
          rules_score_row("Clean sheet", "+8"),
          rules_score_row("Win",         "+5"),
          rules_score_row("Draw",        "+2")
        ),
        div(
          class = "rules-score-col",
          rules_score_row("Penalty save (in play)", "+3"),
          rules_score_row("Assist",        "+6"),
          rules_score_row("Goal conceded", "−2", neg = TRUE)
        )
      )
    ),
    
    # Scoring clarifications ------------------------------------------
    div(
      class = "panel-card rules-section",
      div(class = "rules-section-label", "Scoring clarifications"),
      rules_item("Flat scoring.",
                 "Every gameweek scores the same — there are no round multipliers,
         bonus fixtures, or scoring bumps of any kind."),
      rules_item("Goals stack.",
                 "A goal also counts as a shot and a shot on target, so every goal is
         worth 12 points in total (10 + 1 + 1)."),
      rules_item("Own goals.",
                 "An own goal scores ", span(class = "rules-neg", "−5"),
                 " for the player who scores it, and counts as a goal conceded by
         their team for clean-sheet and goalkeeper purposes."),
      rules_item("Cards.",
                 "A straight red scores −5. A second yellow is scored as a red card,
         added to the first yellow: −1.5 − 5 = ",
                 span(class = "rules-neg", "−6.5"), "."),
      rules_item("Goalkeepers.",
                 "Goalkeepers also score all standard points (goals, shots,
         assists, tackles, interceptions, cards) in addition to the
         goalkeeper-specific points listed."),
      rules_item("Clean sheets.",
                 "The team must concede no goals across the entire match, and the
         player must play more than 60 minutes. A player substituted off
         before a goal is conceded does not retain the clean sheet.")
    ),
    
    # Stats, positions & withdrawals -----------------------------------
    div(
      class = "panel-card rules-section",
      div(class = "rules-section-label", "Stats, positions & withdrawals"),
      rules_item("Data source.",
                 "All match statistics are drawn automatically from TheStatsAPI, the
         authoritative source for scoring. If the provider corrects a
         statistic, scores and standings update accordingly. Final standings
         are confirmed 48 hours after the last Gameweek 19 fixture, after
         which they are fixed for prize purposes."),
      rules_item("Positions.",
                 "Player positions as listed in the app at lock time are final for
         scoring purposes, regardless of where a player actually plays."),
      rules_item("Withdrawals.",
                 "There are no replacements after lock for any reason — injury,
         suspension, transfer, or non-selection. Players remain on your
         roster and simply score whatever they score.")
    ),
    
    # Leaderboard ------------------------------------------------------
    div(
      class = "panel-card rules-section",
      div(class = "rules-section-label", "Leaderboard"),
      p(class = "rules-body",
        "The leaderboard is first published once Gameweek 1 is complete,
         and updated regularly thereafter. Scores may be provisional while
         match stats are collated.")
    ),
    
    # Prize structure --------------------------------------------------
    div(
      class = "panel-card rules-section",
      div(class = "rules-section-label", "Prize structure"),
      p(class = "rules-body",
        "There is no rake — 100% of the pot is paid out as prizes. The pot
         is the sum of all paid £10 entries. While the field is under 50
         entries, the top 10% of entries are paid (rounded up, minimum 2,
         maximum 5 places). From 50 entries, the top 15% are paid (rounded
         up)."),
      div(
        class = "rules-pill-row",
        rules_pill("2 places ·",  "65 / 35"),
        rules_pill("3 places ·",  "50 / 30 / 20"),
        rules_pill("4+ places ·", "1st takes 40%")
      ),
      rules_item("Payout curve.",
                 "Prizes descend smoothly from 1st to the lowest paid place, and no
         prize is less than 10% of the 1st prize. The final prize structure
         is confirmed after lock, once the final number of paid entries is
         known."),
      rules_item("Ties.",
                 "Entries level on points share the combined prize money for the tied
         places equally.")
    ),
    
    # Payment -----------------------------------------------------------
    div(
      class = "panel-card rules-section",
      div(class = "rules-section-label", "Payment"),
      p(class = "rules-body",
        "Entries are £10 each, paid in sterling via Revolut. Payment status
         is updated manually by the admin, so there may be a short lag
         between paying and being marked as paid."),
      p(class = "rules-body",
        span(class = "rules-neg", "Unpaid entries at lock are void"),
        " — removed from the leaderboard, not eligible for prizes, and not
         counted toward the field size."),
      payment_link(label = "Pay via Revolut →",
                   class = "btn rules-pay-btn")
    ),
    
    # Governance ---------------------------------------------------------
    div(
      class = "panel-card panel-card-dark rules-section",
      div(class = "rules-section-label", "Governance"),
      p(class = "rules-body",
        "In the event of any unforeseen circumstance, dispute, scoring
         ambiguity, or matter not covered by these rules, the commissioner
         will make a final decision, which is binding on all entrants.")
    )
  )
}