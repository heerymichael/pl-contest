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
      "Build a roster of 11 players from across all 48 qualified nations and
       accumulate points from their real-life performances throughout the
       tournament."),
    
    # Roster construction --------------------------------------------
    div(
      class = "panel-card panel-card-dark rules-section",
      div(class = "rules-section-label", "Roster construction"),
      div(
        class = "rules-chip-row",
        rules_chip("GK",  "exactly 1"),
        rules_chip("DEF", "3–5"),
        rules_chip("MID", "3–6"),
        rules_chip("FWD", "1–4")
      ),
      div(
        class = "rules-pill-row",
        span(class = "rules-meta-pill", "11 players total"),
        span(class = "rules-meta-pill", "Max 1 per nation"),
        span(class = "rules-meta-pill", "Up to 10 entries")
      )
    ),
    
    # Lock time -------------------------------------------------------
    div(
      class = "panel-card rules-section",
      div(class = "rules-section-label", "Lock time"),
      span(class = "rules-lock-chip", "Locks 1 hour before the opening kickoff"),
      p(class = "rules-body",
        "After lock, no further edits are possible. Players whose nations are
         eliminated remain on your roster but stop accumulating points.")
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
          rules_score_row("Shootout goal",  "+1.5")
        ),
        div(
          class = "rules-score-col",
          rules_score_row("Clean sheet (DEF)", "+6"),
          rules_score_row("Clean sheet (MID)", "+4"),
          rules_score_row("Yellow card",   "−1.5", neg = TRUE),
          rules_score_row("Red card",      "−5",   neg = TRUE),
          rules_score_row("Shootout miss", "−1",   neg = TRUE)
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
          rules_score_row("Assist",      "+6")
        ),
        div(
          class = "rules-score-col",
          rules_score_row("Penalty save (in play)", "+3"),
          rules_score_row("Shootout save",          "+1.5"),
          rules_score_row("Goal conceded", "−2", neg = TRUE)
        )
      )
    ),
    
    # Scoring clarifications ------------------------------------------
    div(
      class = "panel-card rules-section",
      div(class = "rules-section-label", "Scoring clarifications"),
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
         assists, cards) in addition to the goalkeeper-specific points
         listed."),
      rules_item("Clean sheets.",
                 "The team must concede no goals across the entire match — including
         extra time in knockout rounds — and the player must play more than
         60 minutes. A player substituted off before a goal is conceded does
         not retain the clean sheet."),
      rules_item("Extra time.",
                 "All points earned in extra time score normally at the round's
         multiplier."),
      rules_item("Shootouts.",
                 "Only shootout-specific points apply. A miss is any kick that does
         not score, whether saved or off target. Shootout goals do not count
         as goals scored or conceded and do not affect clean sheets."),
      rules_item("Shootout wins.",
                 "A match won on a penalty shootout counts as a win for goalkeeper
         scoring."),
      rules_item("Third place.",
                 "The third-place playoff is fully zeroed: no points of any kind,
         positive or negative, are scored.")
    ),
    
    # Round multipliers ------------------------------------------------
    div(
      class = "panel-card rules-section",
      div(class = "rules-section-label", "Round multipliers"),
      div(
        class = "rules-pill-row",
        rules_pill("Group", "×1.0"),
        rules_pill("R32",   "×1.2"),
        rules_pill("R16",   "×1.4"),
        rules_pill("QF",    "×1.6"),
        rules_pill("SF",    "×1.8"),
        rules_pill("3rd place ×0", class = "rules-pill-muted"),
        rules_pill("Final ×2.0",   class = "rules-pill-final")
      )
    ),
    
    # Stats, positions & withdrawals -----------------------------------
    div(
      class = "panel-card rules-section",
      div(class = "rules-section-label", "Stats, positions & withdrawals"),
      rules_item("Data source.",
                 "All match statistics are drawn automatically from TheStatsAPI, the
         authoritative source for scoring. If the provider corrects a
         statistic, scores and standings update accordingly. Final standings
         are confirmed 48 hours after the final, after which they are fixed
         for prize purposes."),
      rules_item("Positions.",
                 "Player positions as listed in the app at lock time are final for
         scoring purposes, regardless of where a player actually plays."),
      rules_item("Withdrawals.",
                 "There are no replacements after lock for any reason — injury,
         withdrawal, suspension, or non-selection.")
    ),
    
    # Leaderboard ------------------------------------------------------
    div(
      class = "panel-card rules-section",
      div(class = "rules-section-label", "Leaderboard"),
      p(class = "rules-body",
        "The leaderboard is first published once every nation has completed
         its first group-stage match (the end of Matchday 1), and updated
         regularly thereafter. Scores may be provisional while match stats
         are collated.")
    ),
    
    # Prize structure --------------------------------------------------
    div(
      class = "panel-card rules-section",
      div(class = "rules-section-label", "Prize structure"),
      p(class = "rules-body",
        "There is no rake — 100% of the pot is paid out as prizes. The pot is
         the sum of all paid entries. While the field is under 50 entries,
         the top 10% of entries are paid (rounded up, minimum 2, maximum 5
         places). From 50 entries, the top 15% are paid (rounded up)."),
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
    
    # Confirmed prize pool (this contest) ------------------------------
    div(
      class = "panel-card rules-section",
      div(class = "rules-section-label", "Prize pool — this contest"),
      p(class = "rules-body",
        "The field closed at 38 paid entries; unpaid entries were voided at
         lock and excluded from the standings and the count. At $10 per entry
         the pot is $380, paid across the top 4 places (the top 10% of 38,
         rounded up). First place takes 40% of the pot, with prizes descending
         smoothly from there:"),
      div(
        class = "rules-pill-row",
        rules_pill("1st ·", "$152.00"),
        rules_pill("2nd ·", "$102.00"),
        rules_pill("3rd ·", "$74.00"),
        rules_pill("4th ·", "$52.00"),
        rules_pill("Pot ·", "$380", class = "rules-pill-final")
      ),
      rules_item("Currency.",
                 "Entry fees were paid in a mix of US dollars and pounds sterling.
         Prizes are set in dollars at the $10 entry rate. Where a prize is paid
         in sterling it converts at the same $10 : £8 rate used for entries —
         £121.60 / £81.60 / £59.20 / £41.60 (total £304).")
    ),
    
    # Payment -----------------------------------------------------------
    div(
      class = "panel-card rules-section",
      div(class = "rules-section-label", "Payment"),
      p(class = "rules-body",
        "Entries are paid via Revolut. All entry monies are held by
         Revolut (an independent third party) for security — funds are not
         held by the organiser. Payment status is updated manually by the
         admin, so there may be a short lag between paying and being marked
         as paid."),
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