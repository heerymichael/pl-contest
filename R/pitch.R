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
# Entry points:
#   pitch_readonly(picks)        -> squad view, no scores (pre-scoring)
#   pitch_squad(squad)           -> scored view: XI in the position bands,
#                                   non-contributors in a BENCH band, each
#                                   chip carrying its points in a circle
#   pitch_gw_summary(...)        -> the strip above the pitch
#   pitch_gw_selector(...)       -> the gameweek pills
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
# NOTE: column presence is tested with %in% names(p), not p$col. On a
# tibble, p$missing_col emits "Unknown or uninitialised column" — sixty of
# them per render once a 15-player squad is drawn, which buries the
# diagnostic messages that actually matter.
.pitch_name_parts <- function(p) {
  nm    <- if ("name" %in% names(p)) as.character(p$name[1]) else ""
  if (is.na(nm)) nm <- ""
  parts <- strsplit(trimws(nm), "\\s+")[[1]]
  if (length(parts) == 0) parts <- ""
  
  has <- function(col) {
    col %in% names(p) && !is.na(p[[col]][1]) && nzchar(as.character(p[[col]][1]))
  }
  
  sur <- if (has("surname")) as.character(p$surname[1]) else parts[length(parts)]
  
  first <- if (has("first_name")) {
    as.character(p$first_name[1])
  } else if (length(parts) > 1) {
    paste(parts[-length(parts)], collapse = " ")
  } else {
    ""
  }
  
  list(first = first, surname = sur)
}

# Points as they appear in a chip circle: one decimal, always signed
# consistently ("-1.5", "0.0", "12.5").
.pitch_fmt_pts <- function(x) {
  if (length(x) == 0 || is.na(x)) return("\u2014")
  formatC(round(as.numeric(x), 1), format = "f", digits = 1)
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
                              available_gws = integer(0),
                              closed_gws    = integer(0)) {
  
  pill <- function(value, label, enabled, final = FALSE) {
    cls <- paste(c("lb-gw-pill",
                   if (identical(value, active)) "lb-gw-pill--active",
                   if (!enabled) "lb-gw-pill--disabled",
                   if (enabled && !final) "lb-gw-pill--live"),
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
    pill(paste0("gw:", g), paste0("GW", g),
         enabled = g %in% available_gws,
         final   = g %in% closed_gws)
  })
  
  tagList(
    div(
      class = "lb-gw-strip",
      pill("squad", "Opening squad", TRUE, final = TRUE),
      pill("transfers", "Transfers", TRUE, final = TRUE),
      gw_pills
    ),
    if (length(available_gws) == 0) {
      div(class = "lb-gw-note",
          "Gameweek views open once scoring goes live.")
    }
  )
}

# --- Scored squad view ----------------------------------------------------

# The strip between the selector and the pitch. `status` is free text
# ("In progress \u00b7 1 of 10 matches" / "Final"); the totals are the
# selected gameweek's XI and bench points.
pitch_gw_summary <- function(status, xi_points = NA, bench_points = NA) {
  div(
    class = "lb-gw-summary",
    span(class = "lb-gw-summary-status", status),
    if (!is.na(xi_points)) {
      span(
        class = "lb-gw-summary-totals",
        "Best XI ",
        span(class = "lb-gw-summary-xi", .pitch_fmt_pts(xi_points)),
        " \u00b7 bench ", .pitch_fmt_pts(bench_points)
      )
    }
  )
}

# One chip. `pl` is a one-row frame carrying name / team, `pts` the number
# for the circle, `postag` an optional position label (bench chips sit
# outside their band, so without it you can't tell what a benched player
# is \u2014 which matters when working out why a bench outscored an XI).
.pitch_chip <- function(pl, pts = NA, bench = FALSE, postag = NULL,
                        beat_xi = FALSE, hot = FALSE) {
  nm <- .pitch_name_parts(pl)
  tags$div(
    class = paste(c("pitch-slot", "pitch-slot--filled", "pitch-slot--scored",
                    if (bench) "pitch-slot--bench",
                    if (beat_xi) "pitch-slot--beat"),
                  collapse = " "),
    title = if (beat_xi)
      "Outscored a selected player but couldn't be picked \u2014 the formation limits blocked it"
    else NULL,
    if (!is.null(postag)) tags$span(class = "pitch-slot-postag", postag),
    pitch_badge_ro(if ("team" %in% names(pl)) pl$team[1] else NA),
    # Single-name players (Costinha, Ederson) get one centred line rather
    # than a blank row where a first name would be. Chip height is set by
    # the 30px crest and score disc either way, so the grid stays aligned.
    tags$span(
      class = paste(c("pitch-player", "pitch-player--stacked",
                      if (!nzchar(nm$first)) "pitch-player--single"),
                    collapse = " "),
      if (nzchar(nm$first))
        tags$span(class = "pitch-player-first", nm$first),
      tags$span(class = "pitch-player-surname", nm$surname)
    ),
    if (!is.na(pts)) tags$span(
      class = paste(c("pitch-score", if (hot) "pitch-score--hot"),
                    collapse = " "),
      .pitch_fmt_pts(pts),
      if (hot) tags$span(
        class = "pitch-flame",
        title = "Top five in this position across the league this gameweek",
        "\U0001F525"
      )
    )
  )
}

# squad: data frame with player_id, name, team, position, and optionally
#        points and in_xi.
#
# When in_xi is present and not all NA, the XI is drawn in the position
# bands and everyone else in a BENCH band \u2014 so the bands show the
# formation actually fielded. When in_xi is absent or all NA (the
# opening-squad view), all 15 are drawn in their bands with no bench, because
# there is no single XI spanning a season.
# `hot_ids` marks players inside the league's top five at their position for
# the selected gameweek — see bestball_hot_players() in R/bestball.R.
pitch_squad <- function(squad,
                        empty_message = "This entry has no players.",
                        hot_ids = character(0)) {
  
  if (is.null(squad) || nrow(squad) == 0) {
    return(div(class = "empty-state", p(empty_message)))
  }
  
  has_pts <- "points" %in% names(squad)
  if (!has_pts) squad$points <- NA_real_
  split_xi <- "in_xi" %in% names(squad) && any(!is.na(squad$in_xi))
  if (!split_xi) squad$in_xi <- TRUE
  
  squad$position <- as.character(squad$position)
  squad$in_xi    <- ifelse(is.na(squad$in_xi), FALSE, as.logical(squad$in_xi))
  
  # A benched player who outscored someone in the XI can only happen when a
  # formation cap blocked the swap. Rare, and worth marking rather than
  # leaving to look like a bug.
  min_xi <- if (split_xi && any(squad$in_xi) && has_pts)
    min(squad$points[squad$in_xi], na.rm = TRUE) else NA_real_
  
  band <- function(pos) {
    rows <- squad[squad$in_xi & squad$position == pos, , drop = FALSE]
    if (nrow(rows) == 0) return(NULL)
    rows <- rows[order(-rows$points, rows$name), , drop = FALSE]
    chips <- lapply(seq_len(nrow(rows)), function(i) {
      .pitch_chip(rows[i, , drop = FALSE], rows$points[i],
                  hot = as.character(rows$player_id[i]) %in% hot_ids)
    })
    tags$div(
      class = paste0("pitch-row pitch-row--", tolower(pos)),
      tags$span(class = "pitch-row-label", pos),
      tags$div(class = "pitch-row-slots pitch-row-slots--grid", chips)
    )
  }
  
  bands <- Filter(Negate(is.null), lapply(PITCH_BANDS, band))
  
  bench_band <- NULL
  if (split_xi) {
    bench <- squad[!squad$in_xi, , drop = FALSE]
    if (nrow(bench) > 0) {
      bench$pos_ord <- match(bench$position, PITCH_BANDS)
      bench <- bench[order(bench$pos_ord, -bench$points, bench$name), ,
                     drop = FALSE]
      chips <- lapply(seq_len(nrow(bench)), function(i) {
        .pitch_chip(bench[i, , drop = FALSE], bench$points[i], bench = TRUE,
                    postag  = bench$position[i],
                    beat_xi = !is.na(min_xi) && !is.na(bench$points[i]) &&
                      bench$points[i] > min_xi,
                    hot     = as.character(bench$player_id[i]) %in% hot_ids)
      })
      bench_band <- tags$div(
        class = "pitch-row pitch-row--bench",
        tags$span(class = "pitch-row-label", "BENCH"),
        tags$div(class = "pitch-row-slots pitch-row-slots--grid", chips)
      )
    }
  }
  
  if (length(bands) == 0 && is.null(bench_band)) {
    return(div(class = "empty-state", p(empty_message)))
  }
  div(class = "pitch pitch--readonly pitch--scored",
      bands, bench_band)
}