# Leaderboard: ranked list of all entries (left) with a click-through
# squad panel (right) showing the selected entry's 15 players on the
# banded pitch view, with a gameweek selector above it.
#
# Scoring is LIVE: points are computed on the fly from the
# player_match_stats tab (5-min memoised read) through the pure engine
# in R/scoring.R — nothing is precomputed or stored, so a scoring
# correction fixes all history on the next render. Winnings are not
# computed or displayed at all right now — the prize structure is
# field-size dependent and gets finalised post-lock (see the note
# above LB_PRIZES). If scoring ever fails, the board logs the error
# and renders pre-scoring em-dashes rather than crashing.
#
# Roster privacy: rosters are hidden until the contest locks, except
# that a logged-in user can always see their own entries' rosters
# (they can already see those in My Lineups, so nothing is leaked).

message(">>> SOURCED R/leaderboard.R at ", format(Sys.time()))

library(shiny)
library(dplyr)

# --- Scoring (wired to R/scoring.R over player_match_stats) -------------

# Entries voided at lock (unpaid). Removed from the leaderboard, the
# ranking, and prize allocation — per the rules, void entries don't count
# toward the field. Maintained manually by the commissioner.
VOID_ENTRY_IDS <- c(
  "72ee937b-0d41-4d5f-bc03-002482ca021d",
  "863344bd-51cc-41c6-9e1d-c22ed70b5068"
)

# Prize structure is field-size dependent (see Rules -> Prize structure:
# top 10%/15% of paid entries, tiers by field size) and can only be
# finalised once entries lock and the paid field is known. Winnings are
# deliberately NOT computed or displayed yet — see lb_entry_winnings()
# below, removed for now. Re-add once the structure is confirmed
# post-lock.

# Total points per entry, aligned to the rows of `entries`.
# `scored` is the output of score_player_match_stats(), or NULL when no
# stats exist yet / scoring failed — in which case every entry gets NA
# and the UI renders pre-scoring em-dashes.
lb_entry_points <- function(entries, scored, all_picks) {
  if (is.null(scored) || nrow(scored) == 0) return(rep(NA_real_, nrow(entries)))
  per_entry <- entry_player_points(scored, all_picks) |>
    group_by(entry_id) |>
    summarise(total = sum(points), .groups = "drop")
  out <- per_entry$total[match(as.character(entries$entry_id),
                               per_entry$entry_id)]
  ifelse(is.na(out), 0, out)
}

# lb_entry_winnings() removed — see note above LB_PRIZES. Winnings are not
# computed or displayed until the prize structure is confirmed post-lock.

# Per-player aggregate stats for a roster: one row per pick, same order
# as `picks`. `scored` as above; NULL renders pre-scoring em-dashes,
# otherwise players with no appearances show zeros.
lb_player_stats <- function(picks, scored) {
  n <- nrow(picks)
  if (is.null(scored) || nrow(scored) == 0) {
    return(tibble::tibble(
      mp      = rep(NA_integer_, n),
      minutes = rep(NA_integer_, n),
      goals   = rep(NA_integer_, n),
      shots   = rep(NA_integer_, n),
      assists = rep(NA_integer_, n),
      pts     = rep(NA_real_,    n)
    ))
  }
  agg <- scored |>
    group_by(player_id) |>
    summarise(mp      = dplyr::n(),
              minutes = sum(minutes_played),
              goals   = sum(goals),
              shots   = sum(total_shots),
              assists = sum(assists),
              pts     = sum(points),
              .groups = "drop")
  idx <- match(as.character(picks$player_id), agg$player_id)
  tibble::tibble(
    mp      = ifelse(is.na(idx), 0L, agg$mp[idx]),
    minutes = ifelse(is.na(idx), 0L, agg$minutes[idx]),
    goals   = ifelse(is.na(idx), 0L, agg$goals[idx]),
    shots   = ifelse(is.na(idx), 0L, agg$shots[idx]),
    assists = ifelse(is.na(idx), 0L, agg$assists[idx]),
    pts     = ifelse(is.na(idx), 0,  agg$pts[idx])
  )
}

# --- Rank labels ---------------------------------------------------------

# Standard competition ranking with a "T" prefix for tied groups
# (1, 2, T3, T3, 5, ...). Assumes `points` is already sorted
# descending with NAs last. NA points get an em-dash. Points are
# rounded to 1dp before comparison so float artifacts from the 0.2 /
# 0.4 / etc. round multipliers can't split a genuine tie.
lb_rank_labels <- function(points) {
  n      <- length(points)
  labels <- rep("\u2014", n)
  has    <- !is.na(points)
  if (!any(has)) return(labels)
  
  pts   <- round(points[has], 1)
  ranks <- match(pts, pts)                      # first index = comp. rank
  tied  <- ave(pts, pts, FUN = length) > 1
  labels[has] <- ifelse(tied, paste0("T", ranks), as.character(ranks))
  labels
}

# Format a points value for display ("141.2", integer-safe, NA -> dash).
lb_fmt_pts <- function(x) {
  ifelse(is.na(x), "\u2014", formatC(round(x, 1), format = "f", digits = 1))
}

# Format an integer stat for display (NA -> dash).
lb_fmt_int <- function(x) {
  ifelse(is.na(x), "\u2014", format(as.integer(x)))
}

# --- "Includes matches up to" strip ---------------------------------------

# Built from the most recent match in player_match_stats (ISO utc_date
# strings sort lexicographically), with both teams' flags. Returns NULL
# when no stats exist yet, so pre-scoring boards show nothing extra.
# Sides are ordered winner-first (arbitrary on a draw).
lb_latest_match_strip <- function(stats) {
  if (is.null(stats) || nrow(stats) == 0) return(NULL)
  if (!all(c("match_id", "utc_date", "team", "team_goals_for") %in%
           names(stats))) return(NULL)
  
  latest_id <- stats$match_id[order(as.character(stats$utc_date),
                                    decreasing = TRUE)][1]
  m <- stats[stats$match_id == latest_id, ]
  sides <- unique(m[, c("team", "team_goals_for")])
  if (nrow(sides) != 2) return(NULL)
  sides <- sides[order(-sides$team_goals_for), ]
  
  date_txt <- tryCatch(
    format(as.Date(substr(as.character(m$utc_date[1]), 1, 10)), "%d %b"),
    error = function(e) ""
  )
  
  div(
    class = "lb-latest",
    span(class = "lb-latest-label", "Includes matches up to:"),
    span(
      class = "lb-latest-match",
      badge_tag(sides$team[1], size = "sm"),
      span(class = "lb-latest-score",
           paste0(sides$team[1], " ", sides$team_goals_for[1], "\u2013",
                  sides$team_goals_for[2], " ", sides$team[2])),
      badge_tag(sides$team[2], size = "sm"),
      span(class = "lb-latest-date", date_txt)
    )
  )
}

# --- Player avatars -------------------------------------------------------

# Circle-cropped, dark-ringed headshot. Looks for a local image at
# www/headshots/{player_id}.png (or .jpg); if none exists, renders a
# brand-styled initials circle instead. Drop image files into
# www/headshots/ at any time — they're picked up automatically with
# no code changes (cache-busted by mtime like styles.css).
player_avatar <- function(player_id, name) {
  for (ext in c("png", "jpg")) {
    rel  <- file.path("headshots", paste0(player_id, ".", ext))
    disk <- file.path("www", rel)
    if (file.exists(disk)) {
      return(tags$img(
        class = "player-avatar",
        src   = paste0(rel, "?v=", as.integer(file.mtime(disk))),
        alt   = name
      ))
    }
  }
  parts    <- strsplit(trimws(name), "\\s+")[[1]]
  initials <- toupper(paste0(
    substr(parts[1], 1, 1),
    if (length(parts) > 1) substr(parts[length(parts)], 1, 1) else ""
  ))
  span(class = "player-avatar player-avatar-initials", initials)
}

# --- VIEW ----------------------------------------------------------------

leaderboard_view <- function() {
  div(
    class = "view",
    div(
      class = "lineup-header",
      div(
        div(class = "lineup-eyebrow", "Premier League 26/27"),
        h2("Leaderboard")
      )
    ),
    div(class = "lb-status", uiOutput("lb_status_line")),
    fluidRow(
      column(
        5,
        div(
          class = "panel-card lb-board-card",
          wc_spinner(uiOutput("lb_table"))
        )
      ),
      column(
        7,
        div(
          class = "panel-card panel-card-dark lb-roster-card",
          wc_spinner(uiOutput("lb_roster"))
        ),
        div(
          class = "panel-card lb-own-card",
          uiOutput("lb_ownership_head"),
          wc_spinner(uiOutput("lb_ownership_body"))
        )
      )
    )
  )
}

# --- SERVER LOGIC ---------------------------------------------------------

leaderboard_server_logic <- function(input, output, session,
                                     current_user, view_state,
                                     is_locked_reactive) {
  
  message(">>> ENTER leaderboard_server_logic")
  
  lb_selected_id <- reactiveVal(NULL)
  
  # Scored player-match stats. NULL when the stats tab is empty or
  # scoring fails (logged loudly); both render as em-dashes downstream.
  lb_scored <- reactive({
    if (view_state() != "leaderboard") return(NULL)
    stats <- get_match_stats_cached()
    if (is.null(stats) || nrow(stats) == 0) return(NULL)
    tryCatch(
      score_player_match_stats(stats, get_players_all_local()),
      error = function(e) {
        message(">>> [leaderboard] *** SCORING FAILED — board renders ",
                "dashes *** : ", conditionMessage(e))
        NULL
      }
    )
  })
  
  # Ranked entries. Reads the entries tab once per visit to the
  # leaderboard view (the reactive re-runs when view_state changes,
  # and its result is cached for both outputs below).
  lb_data <- reactive({
    if (view_state() != "leaderboard") return(NULL)
    message(">>> [leaderboard] loading entries")
    
    # Prefer local snapshots (instant); fall back to live Sheets reads
    # if snapshot_contest_data.R hasn't been run.
    entries <- get_entries_local()
    if (is.null(entries)) {
      message("    [leaderboard] no entries snapshot — live Sheets read")
      entries <- get_all_entries()
    }
    if (nrow(entries) == 0) return(entries)
    
    # Drop voided (unpaid) entries — off the board, out of the ranking, not
    # eligible for prizes, not counted toward the field (see VOID_ENTRY_IDS).
    entries <- entries |> filter(!entry_id %in% VOID_ENTRY_IDS)
    if (nrow(entries) == 0) return(entries)
    
    all_picks <- get_all_picks_local()
    if (is.null(all_picks)) {
      message("    [leaderboard] no picks snapshot — live Sheets read")
      all_picks <- get_all_roster_picks()
    }
    
    entries$points <- lb_entry_points(entries, lb_scored(), all_picks)
    entries <- entries |>
      arrange(desc(points), display_name, entry_name)
    entries$rank_label <- lb_rank_labels(entries$points)
    
    message("    [leaderboard] ", nrow(entries), " entries loaded")
    entries
  })
  
  # Auto-select the top entry whenever the board loads (or the
  # current selection disappears), so the roster panel is never empty.
  observe({
    rows <- lb_data()
    if (is.null(rows) || nrow(rows) == 0) {
      lb_selected_id(NULL)
      return()
    }
    sel <- lb_selected_id()
    if (is.null(sel) || !sel %in% rows$entry_id) {
      lb_selected_id(rows$entry_id[1])
    }
  })
  
  # Status line under the header ------------------------------------
  
  output$lb_status_line <- renderUI({
    rows <- lb_data()
    if (is.null(rows)) return(NULL)
    if (nrow(rows) == 0 || all(is.na(rows$points))) {
      span("Scoring goes live after Gameweek 1 \u2014 points
            will appear here as matches are scored.")
    } else {
      span("Scores may be provisional while match stats are collated.")
    }
  })
  
  # Board (left) ------------------------------------------------------
  
  output$lb_table <- renderUI({
    rows <- lb_data()
    if (is.null(rows)) return(NULL)
    
    if (nrow(rows) == 0) {
      return(div(class = "empty-state",
                 p("No entries yet \u2014 the leaderboard will fill up as
                    lineups are submitted.")))
    }
    
    sel_id <- lb_selected_id()
    
    latest_strip <- lb_latest_match_strip(get_match_stats_cached())
    
    header <- div(
      class = "lb-row lb-row-header",
      span(class = "lb-rank",    "#"),
      span(class = "lb-manager", "Manager"),
      span(class = "lb-entry",   "Entry"),
      span(class = "lb-pts",     "Pts")
      # Winnings column removed for now — prize structure is
      # field-size dependent, confirmed post-lock. See note above
      # LB_PRIZES in the reactive section.
    )
    
    row_uis <- lapply(seq_len(nrow(rows)), function(i) {
      r <- rows[i, ]
      is_selected <- !is.null(sel_id) && sel_id == r$entry_id
      row_class <- paste("lb-row lb-row-click",
                         if (is_selected) "lb-row-selected")
      div(
        class   = row_class,
        onclick = sprintf(
          "Shiny.setInputValue('lb_select_entry', '%s', {priority: 'event'});",
          r$entry_id),
        span(class = "lb-rank",    r$rank_label),
        span(class = "lb-manager", r$display_name),
        span(class = "lb-entry",   r$entry_name),
        span(class = "lb-pts",     lb_fmt_pts(r$points))
        # Winnings cell removed for now — see header comment above.
      )
    })
    
    tagList(latest_strip, header, row_uis)
  })
  
  observeEvent(input$lb_select_entry, {
    target <- input$lb_select_entry
    if (is.null(target) || !nzchar(target)) return()
    message(">>> OBSERVE lb_select_entry — ", target)
    lb_selected_id(target)
  })
  
  # Roster panel (right) ----------------------------------------------
  
  output$lb_roster <- renderUI({
    rows <- lb_data()
    eid  <- lb_selected_id()
    if (is.null(rows) || nrow(rows) == 0 || is.null(eid)) {
      return(div(class = "lb-roster-empty",
                 p("Select an entry to see its roster.")))
    }
    
    entry <- rows |> filter(entry_id == eid)
    if (nrow(entry) != 1) return(NULL)
    
    # Privacy gate: rosters are revealed at lock. A user can always
    # see their own entries (already visible in My Lineups).
    user   <- current_user()
    own    <- !is.null(user) && identical(user$email, entry$user_email)
    locked <- is_locked_reactive()
    
    panel_header <- tagList(
      div(class = "lb-roster-eyebrow",
          paste0(entry$display_name, " \u00b7 ", lb_fmt_pts(entry$points),
                 " pts")),
      h3(class = "lb-roster-title", entry$entry_name)
    )
    
    if (!locked && !own) {
      return(tagList(
        panel_header,
        div(class = "lb-roster-empty",
            p("Rosters are revealed when the contest locks."))
      ))
    }
    
    picks <- get_picks_for_entry_local(eid)
    if (is.null(picks)) {
      message("    [leaderboard] no picks snapshot — live Sheets read")
      picks <- get_picks_for_entry(eid)
    }
    if (nrow(picks) == 0) {
      return(tagList(
        panel_header,
        div(class = "lb-roster-empty", p("No picks found for this entry."))
      ))
    }
    
    picks <- picks |>
      mutate(pos_ord = match(position, c("GK", "DEF", "MID", "FWD"))) |>
      arrange(pos_ord, name)
    stats <- lb_player_stats(picks, lb_scored())
    
    # Full club names for the stats list. picks$team carries the
    # short_code; the teams tab is the source of truth for the display
    # name. Unknown codes fall back to the code itself rather than NA.
    team_names <- tryCatch({
      tm <- get_teams_cached()
      setNames(as.character(tm$name), as.character(tm$short_code))
    }, error = function(e) character(0))
    club_name <- function(code) {
      code <- as.character(code)
      nm   <- unname(team_names[code])
      if (length(nm) == 0 || is.na(nm)) code else nm
    }
    
    stats_header <- div(
      class = "lb-roster-row lb-roster-row-header",
      span(class = "lb-roster-pos", ""),
      div(class = "lb-roster-player", ""),
      span(class = "lb-roster-stat", "MP"),
      span(class = "lb-roster-stat lb-rs-min",   "Min"),
      span(class = "lb-roster-stat", "G"),
      span(class = "lb-roster-stat lb-rs-shots", "Sh"),
      span(class = "lb-roster-stat", "A"),
      span(class = "lb-roster-stat lb-roster-stat-pts", "Pts")
    )
    
    row_uis <- lapply(seq_len(nrow(picks)), function(i) {
      p <- picks[i, ]
      s <- stats[i, ]
      # First row of each positional group gets extra top spacing, so
      # GK / DEF / MID / FWD read as blocks rather than one long list.
      group_start <- i > 1 && !identical(p$position, picks$position[i - 1])
      div(
        class = paste(c("lb-roster-row",
                        if (group_start) "lb-roster-row--group-start"),
                      collapse = " "),
        span(class = "lb-roster-pos", p$position),
        div(
          class = "lb-roster-player",
          club_crest(p$team),
          div(
            class = "lb-roster-id",
            span(class = "lb-roster-name", p$name),
            span(
              class = "lb-roster-sub",
              span(class = "lb-roster-team", club_name(p$team))
            )
          )
        ),
        span(class = "lb-roster-stat", lb_fmt_int(s$mp)),
        span(class = "lb-roster-stat lb-rs-min",   lb_fmt_int(s$minutes)),
        span(class = "lb-roster-stat", lb_fmt_int(s$goals)),
        span(class = "lb-roster-stat lb-rs-shots", lb_fmt_int(s$shots)),
        span(class = "lb-roster-stat", lb_fmt_int(s$assists)),
        span(class = "lb-roster-stat lb-roster-stat-pts", lb_fmt_pts(s$pts))
      )
    })
    
    # Gameweek strip + pitch above the per-player stats list.
    # Only "Opening squad" is selectable today: gameweek views need
    # bestball wired into lb_data() (R/bestball.R is implemented but
    # not yet called), so the pills render disabled rather than
    # pretending to work. available_gws stays empty until then.
    cfg      <- tryCatch(get_config_cached(), error = function(e) NULL)
    first_gw <- as.integer(cfg$gameweek_first %||% 1L)
    last_gw  <- as.integer(cfg$gameweek_last  %||% 19L)
    
    selector <- pitch_gw_selector(
      input_id      = "lb_gw_select",
      active        = "squad",
      first_gw      = first_gw,
      last_gw       = last_gw,
      available_gws = integer(0)
    )
    
    tagList(
      panel_header,
      selector,
      pitch_readonly(picks, "No picks found for this entry."),
      div(class = "lb-roster-list", stats_header, row_uis)
    )
  })
  
  # Ownership table (below the roster panel) --------------------------
  
  lb_own_limit <- reactiveVal(10L)
  
  # Ownership across the whole contest: how many entries hold each
  # player, ranked. Built from the same snapshots as the board (or the
  # same live fallbacks), so it costs nothing extra.
  lb_ownership_data <- reactive({
    rows <- lb_data()
    if (is.null(rows) || nrow(rows) == 0) return(NULL)
    
    picks <- get_all_picks_local()
    if (is.null(picks)) picks <- get_all_roster_picks()
    if (nrow(picks) == 0) return(NULL)
    
    players <- get_players_all_local()
    picks |>
      count(player_id, name = "owned") |>
      left_join(players |> select(player_id, name, team, position),
                by = "player_id") |>
      arrange(desc(owned), name) |>
      mutate(pct = owned / nrow(rows))
  })
  
  observeEvent(input$lb_own_more, {
    lb_own_limit(lb_own_limit() + 10L)
  })
  
  # Typing in the search box resets pagination to the first 10 — a new
  # search starts from the top, mirroring the player pool's behaviour.
  observeEvent(input$lb_own_search, {
    lb_own_limit(10L)
  }, ignoreInit = TRUE)
  
  observeEvent(input$lb_own_pos, {
    lb_own_limit(10L)
  }, ignoreInit = TRUE)
  
  # Header + search field + column header. Deliberately does NOT read
  # input$lb_own_search, so typing never re-renders this block and the
  # field keeps focus across keystrokes (the rows re-render separately,
  # below). The inline reset clears any stale filter when the view is
  # rebuilt: the whole page re-renders on nav, recreating an empty box,
  # so the filter value must be cleared to match it.
  output$lb_ownership_head <- renderUI({
    own <- lb_ownership_data()
    if (is.null(own)) return(NULL)
    
    header_block <- tagList(
      div(class = "lb-own-eyebrow", "Contest"),
      h3(class = "lb-own-title", "Player ownership")
    )
    
    # Same privacy gate as rosters: aggregate pick rates would tip off
    # anyone still editing before lock.
    if (!is_locked_reactive()) {
      return(tagList(
        header_block,
        div(class = "empty-state",
            p("Ownership rates are revealed when the contest locks."))
      ))
    }
    
    filters <- div(
      class = "lb-own-filters",
      div(
        class = "lb-own-search",
        HTML(paste0(
          '<svg class="lb-own-search-icon" width="16" height="16" ',
          'viewBox="0 0 24 24" fill="none" stroke="currentColor" ',
          'stroke-width="2" stroke-linecap="round" stroke-linejoin="round" ',
          'aria-hidden="true"><circle cx="11" cy="11" r="7"></circle>',
          '<line x1="20" y1="20" x2="16.65" y2="16.65"></line></svg>'
        )),
        tags$input(
          type         = "text",
          id           = "lb_own_search",
          class        = "lb-own-search-input",
          placeholder  = "Search players\u2026",
          autocomplete = "off",
          oninput      = "Shiny.setInputValue('lb_own_search', this.value);"
        )
      ),
      tags$select(
        id       = "lb_own_pos",
        class    = "lb-own-pos-select",
        onchange = "Shiny.setInputValue('lb_own_pos', this.value);",
        tags$option(value = "",    "All positions"),
        tags$option(value = "GK",  "GK"),
        tags$option(value = "DEF", "DEF"),
        tags$option(value = "MID", "MID"),
        tags$option(value = "FWD", "FWD")
      )
    )
    
    col_header <- div(
      class = "lb-own-row lb-own-row-header",
      span(class = "lb-own-player", "Player"),
      span(class = "lb-own-pos",    "Pos"),
      span(class = "lb-own-count",  "Owned"),
      span(class = "lb-own-pct",    "Own%")
    )
    
    tagList(
      header_block,
      filters,
      tags$script(HTML(
        "if (window.Shiny) { Shiny.setInputValue('lb_own_search', '');
                             Shiny.setInputValue('lb_own_pos', ''); }")),
      col_header
    )
  })
  
  # Rows + pagination footer. Re-renders on each keystroke (reads
  # input$lb_own_search) and on Show-more; the head above stays put.
  # Search matches player name OR team code, case-insensitive.
  output$lb_ownership_body <- renderUI({
    own <- lb_ownership_data()
    if (is.null(own)) return(NULL)
    if (!is_locked_reactive()) return(NULL)   # locked notice lives in the head
    
    q <- input$lb_own_search
    if (!is.null(q) && nzchar(trimws(q))) {
      qq  <- tolower(trimws(q))
      own <- own |>
        filter(grepl(qq, tolower(name), fixed = TRUE) |
                 grepl(qq, tolower(team), fixed = TRUE))
    }
    
    pos <- input$lb_own_pos
    if (!is.null(pos) && nzchar(pos)) {
      own <- own |> filter(position == pos)
    }
    total <- nrow(own)
    
    if (total == 0) {
      return(div(class = "lb-own-empty",
                 p("No players match your filters.")))
    }
    
    shown <- head(own, lb_own_limit())
    
    row_uis <- lapply(seq_len(nrow(shown)), function(i) {
      o <- shown[i, ]
      div(
        class = "lb-own-row",
        div(
          class = "lb-own-player",
          player_avatar(o$player_id, o$name),
          div(
            class = "lb-own-id",
            span(class = "lb-own-name", o$name),
            span(
              class = "lb-own-sub",
              badge_tag(o$team, size = "sm"),
              span(class = "lb-own-team", o$team)
            )
          )
        ),
        span(class = "lb-own-pos",   o$position),
        span(class = "lb-own-count", lb_fmt_int(o$owned)),
        span(class = "lb-own-pct",   sprintf("%.0f%%", 100 * o$pct))
      )
    })
    
    footer <- div(
      class = "pool-pagination-row",
      span(class = "pool-pagination-count",
           sprintf("Showing %d of %d selected players",
                   nrow(shown), total)),
      if (total > nrow(shown)) {
        tags$button(
          class   = "btn btn-default btn-sm pool-show-more-btn",
          onclick = "Shiny.setInputValue('lb_own_more', Math.random(),
                                         {priority: 'event'});",
          "Show 10 more"
        )
      }
    )
    
    tagList(row_uis, footer)
  })
  
  # Reset selection when leaving the view so a stale roster isn't
  # shown on the next visit before the auto-select observer runs.
  observeEvent(view_state(), {
    if (view_state() != "leaderboard") {
      lb_selected_id(NULL)
      lb_own_limit(10L)
    }
  })
}