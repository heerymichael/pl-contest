# R/pitch.R ---------------------------------------------------------------
# Read-only pitch renderer: the banded squad view, without any of the
# builder's editing affordances.
#
# The lineup builder has its own pitch inline in R/lineup_creator.R,
# wired to slot clicks, replace and remove flows through iid()
# namespacing. That version stays exactly where it is — this one is a
# separate, deliberately dumb renderer for contexts that only need to
# DISPLAY a squad (the leaderboard today, potentially My Lineups later).
#
# Both emit the same CSS classes from www/styles.css, so they look
# identical and the 480px compact rules apply to both automatically.
# The duplication is ~40 lines and is the price of not refactoring a
# working 1,300-line module; if a third caller ever appears, that's the
# moment to unify them.
#
# Entry point:
#   pitch_readonly(picks) -> a <div class="pitch pitch--readonly">
#
# `picks` needs: player_id, name, team, position. first_name / surname
# are used when present and derived from `name` when not (which is the
# case for both get_picks_for_entry() and get_picks_for_entry_local(),
# neither of which carries them).
# --------------------------------------------------------------------------

message(">>> SOURCED R/pitch.R at ", format(Sys.time()))

library(shiny)
library(dplyr)

# Band order, top to bottom. Matches the builder (GK deepest at the top).
PITCH_BANDS <- c("GK", "DEF", "MID", "FWD")

# Crest badge. Naked crest SVG, no roundel — sized and shadowed in CSS.
# TOT / LIV / NFO are too dark for the purple panel and use their
# inverted secondary crests. This mirrors pitch_badge() in
# R/lineup_creator.R; keep the two in step if the crest set changes.
pitch_badge_ro <- function(code) {
  code <- as.character(code)
  if (is.na(code) || !nzchar(code)) {
    return(tags$span(class = "pitch-badge"))
  }
  inv  <- toupper(code) %in% c("TOT", "LIV", "NFO")
  file <- paste0(tolower(code), if (inv) "_inv" else "", ".svg")
  tags$span(
    class = "pitch-badge",
    tags$img(src = paste0("logos/", file), alt = code)
  )
}

# Split a display name into (first, surname), preferring explicit
# columns when the caller supplies them.
.pitch_name_parts <- function(p) {
  parts <- strsplit(trimws(as.character(p$name %||% "")), "\\s+")[[1]]
  if (length(parts) == 0) parts <- ""
  
  sur <- if (!is.null(p$surname) && !is.na(p$surname) && nzchar(p$surname)) {
    p$surname
  } else {
    parts[length(parts)]
  }
  
  first <- if (!is.null(p$first_name) && !is.na(p$first_name)) {
    p$first_name
  } else if (length(parts) > 1) {
    paste(parts[-length(parts)], collapse = " ")
  } else {
    ""
  }
  
  list(first = first, surname = sur)
}

# picks: data frame with player_id, name, team, position.
#
# Returns the pitch markup, or an empty-state div when there's nothing
# to show. Players are sorted by name within each band; bands with no
# players are omitted entirely (so a partial roster still renders
# sensibly rather than showing hollow bands).
pitch_readonly <- function(picks,
                           empty_message = "This entry has no players.") {
  
  if (is.null(picks) || nrow(picks) == 0) {
    return(div(class = "empty-state", p(empty_message)))
  }
  
  bands <- lapply(PITCH_BANDS, function(pos) {
    in_band <- picks |>
      filter(.data$position == pos) |>
      arrange(.data$name)
    if (nrow(in_band) == 0) return(NULL)
    
    chips <- lapply(seq_len(nrow(in_band)), function(i) {
      pl <- in_band[i, ]
      nm <- .pitch_name_parts(pl)
      tags$div(
        class = "pitch-slot pitch-slot--filled",
        pitch_badge_ro(pl$team),
        tags$span(
          class = "pitch-player",
          if (nzchar(nm$first))
            tags$span(class = "pitch-player-first", nm$first),
          tags$span(class = "pitch-player-surname", nm$surname)
        )
      )
    })
    
    tags$div(
      class = paste0("pitch-row pitch-row--", tolower(pos)),
      tags$span(class = "pitch-row-label", pos),
      tags$div(class = "pitch-row-slots", chips)
    )
  })
  
  bands <- Filter(Negate(is.null), bands)
  if (length(bands) == 0) {
    return(div(class = "empty-state", p(empty_message)))
  }
  
  div(class = "pitch pitch--readonly", bands)
}

# --- Club crest, avatar-sized -------------------------------------------

# The large circular crest used in the leaderboard stats list, in place
# of the initials avatar. Same footprint as .player-avatar so the row
# grid is unchanged. Dark crests (TOT / LIV / NFO) use their inverted
# variants, since this sits on the dark squad panel. Unknown or missing
# codes render an empty ring rather than a broken image.
club_crest <- function(short_code) {
  code <- toupper(as.character(short_code %||% ""))
  if (!nzchar(code) || is.na(code)) {
    return(tags$span(class = "club-crest club-crest--empty"))
  }
  inv  <- code %in% c("TOT", "LIV", "NFO")
  file <- paste0(tolower(code), if (inv) "_inv" else "", ".svg")
  if (!file.exists(file.path("www", "logos", file))) {
    return(tags$span(class = "club-crest club-crest--empty"))
  }
  tags$span(
    class = "club-crest",
    tags$img(src = paste0("logos/", file), alt = code)
  )
}

# --- Gameweek selector ----------------------------------------------------

# The strip above the pitch. "Opening squad" is the only live option
# until bestball scoring is wired (see R/bestball.R — implemented, not
# yet called from the leaderboard); gameweek pills render disabled so
# the shape of the season is visible without pretending to work.
#
# `active` is the currently selected value ("squad", or "gw:<n>").
# `available_gws` is the vector of gameweeks that can actually be
# selected — empty today. Everything from first_gw:last_gw that isn't
# in `available_gws` renders as a disabled pill.
pitch_gw_selector <- function(input_id,
                              active        = "squad",
                              first_gw      = 1L,
                              last_gw       = 19L,
                              available_gws = integer(0)) {
  
  pill <- function(value, label, enabled) {
    cls <- paste(c("lb-gw-pill",
                   if (identical(value, active)) "lb-gw-pill--active",
                   if (!enabled) "lb-gw-pill--disabled"),
                 collapse = " ")
    tags$button(
      class    = cls,
      type     = "button",
      disabled = if (!enabled) NA else NULL,
      onclick  = if (enabled) sprintf(
        "Shiny.setInputValue('%s', '%s', {priority: 'event'});",
        input_id, value) else NULL,
      label
    )
  }
  
  gw_pills <- lapply(seq.int(first_gw, last_gw), function(g) {
    pill(paste0("gw:", g), paste0("GW", g), g %in% available_gws)
  })
  
  tagList(
    div(
      class = "lb-gw-strip",
      pill("squad", "Opening squad", TRUE),
      gw_pills
    ),
    if (length(available_gws) == 0) {
      div(class = "lb-gw-note",
          "Gameweek views open once scoring goes live.")
    }
  )
}