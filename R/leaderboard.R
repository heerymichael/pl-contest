# Leaderboard: ranked list of all entries (left) with a click-through
# squad panel (right) showing the selected entry's 15 players on the
# banded pitch view, with a gameweek selector above it.
#
# Scoring is LIVE and BESTBALL: points are computed on the fly from the
# player_match_stats tab (5-min memoised read) through the pure engine
# in R/scoring.R, given a gameweek by R/gameweeks.R, then reduced to
# each entry's optimal XI per gameweek by R/bestball.R. An entry's total
# is the sum of its best XI across every gameweek that has stats — NOT
# the sum of all 15 rostered players, which is what this file used to
# show and which both inflates totals and ranks entries differently.
# Nothing is precomputed or stored, so a scoring correction fixes all
# history on the next render. Winnings are not
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

# Temporary notice under the leaderboard status line. Set to "" to remove
# it once every entry is paid and the prize structure is published.
#
# It lives here, next to VOID_ENTRY_IDS and the prize note, rather than
# buried in the renderUI: this is copy with an expiry date, and the failure
# mode for that kind of string is quietly outliving its usefulness on a
# public board.
LB_NOTICE <- paste(
  "Final payments are being chased up \u2014 the prize pool will be",
  "confirmed once the paid field is settled."
)

# Prize structure is field-size dependent (see Rules -> Prize structure:
# top 10%/15% of paid entries, tiers by field size) and can only be
# finalised once entries lock and the paid field is known. Winnings are
# deliberately NOT computed or displayed yet — see lb_entry_winnings()
# below, removed for now. Re-add once the structure is confirmed
# post-lock.

# Total points per entry, aligned to the rows of `entries`.
# `gw_points` is the output of bestball_gameweek_points() — one row per
# (entry_id, gameweek) with that gameweek's optimal-XI points. NULL when
# no stats exist yet / scoring or bestball failed, in which case every
# entry gets NA and the UI renders pre-scoring em-dashes.
#
# An entry present in `entries` but absent from `gw_points` scores 0, not
# NA: that is a real zero (no rostered player has scored) rather than a
# missing measurement.
lb_entry_points <- function(entries, gw_points) {
  if (is.null(gw_points) || nrow(gw_points) == 0) {
    return(rep(NA_real_, nrow(entries)))
  }
  totals <- aggregate(xi_points ~ entry_id, data = gw_points, FUN = sum)
  out <- totals$xi_points[match(as.character(entries$entry_id),
                                as.character(totals$entry_id))]
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

# --- Transfers view --------------------------------------------------------

# The squad panel's Transfers tab: the selected entry's rows from the
# `transfers` audit log (commissioner corrections, window transfers,
# club-clash auto-subs), newest first. Player names and clubs resolve at
# render time from the players tab — the log stores ids only, same
# principle as everywhere else. An entry with no history gets an explicit
# empty state rather than a hidden tab: proving absence is part of the
# transparency.
#
# `transfers` is the entry's rows (already filtered), `players` the full
# players tab, `club_name` the short_code -> display name closure built
# in the renderUI.
lb_transfers_panel <- function(transfers, players, club_name) {
  if (is.null(transfers) || nrow(transfers) == 0) {
    return(div(class = "lb-transfer-empty",
               p("No transfers for this entry.")))
  }
  
  transfers <- transfers |>
    arrange(desc(as.character(created_at)),
            desc(suppressWarnings(as.integer(effective_gw))))
  
  player_row <- function(pid, dir) {
    pl <- players |> filter(player_id == as.character(pid))
    nm <- if (nrow(pl) == 1) pl$name else paste0("Player ", pid)
    tm <- if (nrow(pl) == 1) pl$team else NA_character_
    div(
      class = "lb-transfer-player",
      span(class = paste0("lb-transfer-dir lb-transfer-dir--",
                          tolower(dir)),
           dir),
      if (!is.na(tm)) club_crest(tm),
      div(
        class = "lb-transfer-id",
        span(class = "lb-transfer-name", nm),
        if (!is.na(tm)) {
          span(class = "lb-transfer-team", club_name(tm))
        }
      )
    )
  }
  
  type_label <- c(commissioner = "Commissioner",
                  window       = "Transfer window",
                  autosub      = "Auto-sub")
  
  rows <- lapply(seq_len(nrow(transfers)), function(i) {
    tr  <- transfers[i, ]
    typ <- tolower(trimws(tr$type %||% ""))
    if (!typ %in% names(type_label)) typ <- "commissioner"
    gw  <- suppressWarnings(as.integer(tr$effective_gw))
    dt  <- as.character(tr$created_at)
    dt  <- if (length(dt) == 0 || is.na(dt)) "" else substr(dt, 1, 10)
    note <- as.character(tr$note)
    note <- if (length(note) == 0 || is.na(note)) "" else trimws(note)
    
    div(
      class = "lb-transfer-row",
      div(
        class = "lb-transfer-meta",
        if (!is.na(gw)) {
          span(class = "lb-transfer-gw", paste0("From GW", gw))
        },
        span(class = paste0("lb-transfer-type lb-transfer-type--", typ),
             type_label[[typ]]),
        if (nzchar(dt)) span(class = "lb-transfer-date", dt)
      ),
      div(
        class = "lb-transfer-players",
        player_row(tr$out_player_id, "OUT"),
        player_row(tr$in_player_id,  "IN")
      ),
      if (nzchar(note)) {
        div(class = "lb-transfer-note", note)
      }
    )
  })
  
  div(class = "lb-transfer-list", rows)
}

# --- "Includes matches up to" strip ---------------------------------------

# Every match scored on the MOST RECENT DAY present in
# player_match_stats, not just the single most recent match \u2014 a
# Saturday afternoon lands three-o'clock kick-offs together, and showing
# one of them implies the others aren't counted yet. Laid out on a
# three-column grid so a full slate reads at a glance.
#
# ISO utc_date strings sort lexicographically, so the day is taken by
# max() on the first ten characters. Sides are ordered home-first where
# `is_home` is available, winner-first otherwise. Returns NULL when no
# stats exist, so pre-scoring boards show nothing extra.
lb_latest_match_strip <- function(stats) {
  if (is.null(stats) || nrow(stats) == 0) return(NULL)
  if (!all(c("match_id", "utc_date", "team", "team_goals_for") %in%
           names(stats))) return(NULL)
  
  day_of <- substr(as.character(stats$utc_date), 1, 10)
  ok     <- !is.na(day_of) & nzchar(day_of)
  if (!any(ok)) return(NULL)
  latest_day <- max(day_of[ok])
  
  day <- stats[ok & day_of == latest_day, , drop = FALSE]
  if (nrow(day) == 0) return(NULL)
  
  has_home <- "is_home" %in% names(day)
  cols     <- c("match_id", "team", "team_goals_for",
                if (has_home) "is_home")
  sides    <- unique(day[, cols, drop = FALSE])
  
  cells <- lapply(unique(sides$match_id), function(mid) {
    m <- sides[sides$match_id == mid, , drop = FALSE]
    if (nrow(m) != 2) return(NULL)
    
    home_first <- FALSE
    if (has_home) {
      hf <- toupper(trimws(as.character(m$is_home))) %in% c("TRUE", "1")
      if (sum(hf) == 1) {
        m <- m[order(!hf), , drop = FALSE]
        home_first <- TRUE
      }
    }
    if (!home_first) {
      m <- m[order(-suppressWarnings(as.numeric(m$team_goals_for))), ,
             drop = FALSE]
    }
    
    div(
      class = "lb-latest-match",
      badge_tag(m$team[1], size = "sm"),
      span(class = "lb-latest-code",  m$team[1]),
      span(class = "lb-latest-score",
           paste0(m$team_goals_for[1], "\u2013", m$team_goals_for[2])),
      span(class = "lb-latest-code",  m$team[2]),
      badge_tag(m$team[2], size = "sm")
    )
  })
  cells <- Filter(Negate(is.null), cells)
  n <- length(cells)
  if (n == 0) return(NULL)
  
  # Scale to the slate. One match on a three-column grid at compact sizing
  # looks like a mistake; ten matches at full size doesn't fit. Columns and
  # type size are both driven by the count, so a midweek single fixture and
  # a Saturday full slate each look deliberate.
  #
  # Four goes to two columns rather than three: 3+1 leaves an orphan.
  cols <- if (n <= 2) n else if (n == 4) 2L else 3L
  size <- if (n <= 2) "lg" else if (n <= 6) "md" else "sm"
  
  date_txt <- tryCatch(format(as.Date(latest_day), "%d %b"),
                       error = function(e) "")
  
  div(
    class = paste0("lb-latest lb-latest--cols", cols, " lb-latest--", size),
    div(
      class = "lb-latest-head",
      span(class = "lb-latest-label", "Includes matches up to"),
      span(class = "lb-latest-date", date_txt)
    ),
    div(class = "lb-latest-grid", cells)
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
  
  # Which view the squad panel is showing: "squad" (opening squad, season
  # totals, no XI) or "gw:<n>". Deliberately NOT reset when the selected
  # entry changes — clicking down the board while holding GW3 is how you
  # compare a gameweek across entries.
  #
  # NULL means "the user hasn't chosen", which is distinct from an explicit
  # "squad": untouched defaults to the latest gameweek with any stats, so
  # the panel opens on what's actually happening, while a deliberate click
  # on Opening squad is respected. Without that distinction the default
  # would immediately overwrite anyone who chose Opening squad.
  lb_gw_view <- reactiveVal(NULL)
  
  observeEvent(input$lb_gw_select, {
    v <- input$lb_gw_select
    if (is.null(v) || !nzchar(v)) return()
    message(">>> OBSERVE lb_gw_select — ", v)
    lb_gw_view(v)
  })
  
  # All roster picks (local snapshot if present, live Sheets read
  # otherwise). Hoisted out of lb_data() because bestball and the
  # ownership table both need them, and one read should serve all three.
  lb_picks <- reactive({
    if (view_state() != "leaderboard") return(NULL)
    picks <- get_all_picks_local()
    if (is.null(picks)) {
      message("    [leaderboard] no picks snapshot — live Sheets read")
      picks <- get_all_roster_picks()
    }
    picks
  })
  
  # Gameweek -> period map, for period-aware roster selection after the
  # September transfer window. NULL when the gameweeks tab is
  # unreadable, which bestball treats as "no period filtering".
  lb_gw_period <- reactive({
    gwt <- tryCatch(get_gameweeks_cached(), error = function(e) NULL)
    if (is.null(gwt) || nrow(gwt) == 0) return(NULL)
    setNames(as.integer(gwt$period), as.character(gwt$gw))
  })
  
  # The roster an entry HELD FOR A GIVEN GAMEWEEK — the period-1 fifteen
  # for GW1-2, the period-2 fifteen from GW3 — resolved to name / team /
  # position. This is what the squad panel must draw a gameweek pitch
  # from. get_picks_for_entry() returns the roster held NOW (highest
  # period), which is right for the season view but wrong for a past
  # gameweek: overlaying a period-1 XI onto a period-2 roster drops the
  # outgoing player's XI chip and shows ten, not eleven.
  #
  # Same roster rule as .bb_entry_roster() in R/bestball.R, so the pitch
  # and the score always agree. Returns NULL whenever the period cannot
  # be resolved (no period column, no map, no rows) — callers fall back
  # to the current roster rather than an empty pitch.
  lb_period_roster <- function(eid, gw) {
    all_picks <- lb_picks()
    gwp       <- lb_gw_period()
    if (is.null(all_picks) || nrow(all_picks) == 0) return(NULL)
    if (is.null(gwp) || !"period" %in% names(all_picks)) return(NULL)
    
    want <- suppressWarnings(as.integer(unname(gwp[as.character(gw)])))
    if (length(want) != 1L || is.na(want)) return(NULL)
    
    mine <- all_picks[as.character(all_picks$entry_id) == as.character(eid), ,
                      drop = FALSE]
    pp   <- suppressWarnings(as.integer(mine$period))
    mine <- mine[!is.na(pp) & pp == want, , drop = FALSE]
    if (nrow(mine) == 0) return(NULL)
    
    message("    [leaderboard] GW", gw, " pitch drawn from period ", want,
            " roster — ", nrow(mine), " picks")
    
    players <- get_players_all_local()
    mine |>
      mutate(player_id = as.integer(player_id)) |>
      distinct(player_id) |>
      left_join(players |> select(player_id, name, team, position),
                by = "player_id") |>
      mutate(pid = as.character(player_id)) |>
      select(pid, player_id, name, team, position)
  }
  
  # The club each player was AT for a given gameweek. The players table
  # holds the club NOW, so after a deadline-day move (Ndiaye EVE -> MCI)
  # a GW2 pitch drawn from it shows two City crests in a one-per-club
  # squad. player_match_stats records the club on every appearance, so
  # the club is taken from the player's most recent stat row at or before
  # the selected gameweek. Players with no appearance yet keep the
  # players-table club — there is nothing better to go on.
  #
  # Display only. Nothing here touches scoring.
  lb_historic_clubs <- function(squad, scored, gw) {
    if (is.null(scored) || nrow(scored) == 0) return(squad)
    if (!all(c("player_id", "team", "gameweek") %in% names(scored))) return(squad)
    
    g <- suppressWarnings(as.numeric(scored$gameweek))
    s <- scored[!is.na(g) & g <= gw & !is.na(scored$team), , drop = FALSE]
    if (nrow(s) == 0) return(squad)
    
    s$player_id <- as.character(s$player_id)
    s$gameweek  <- suppressWarnings(as.numeric(s$gameweek))
    s <- s[order(s$player_id, -s$gameweek), , drop = FALSE]
    s <- s[!duplicated(s$player_id), c("player_id", "team"), drop = FALSE]
    
    then <- setNames(as.character(s$team), s$player_id)
    hit  <- squad$pid %in% names(then)
    moved <- hit & squad$team != unname(then[squad$pid])
    if (any(moved)) {
      message("    [leaderboard] GW", gw, " pitch: club restored for ",
              sum(moved), " player(s) — ",
              paste(squad$name[moved], collapse = ", "))
    }
    squad$team[hit] <- unname(then[squad$pid[hit]])
    squad
  }
  
  # Scored player-match stats, with a gameweek on every row. NULL when
  # the stats tab is empty or scoring fails (logged loudly); both render
  # as em-dashes downstream.
  #
  # Stat rows for a player_id the app doesn't know are DROPPED rather
  # than allowed to reach score_player_match_stats(), which stops hard on
  # an unresolvable position. That strictness is right in the ingest,
  # where a human sees the console; here it would turn the whole board
  # into dashes because one player is unmapped. A dropped row scores for
  # nobody, so the loud message is the thing that matters.
  lb_scored <- reactive({
    if (view_state() != "leaderboard") return(NULL)
    stats <- get_match_stats_cached()
    if (is.null(stats) || nrow(stats) == 0) return(NULL)
    
    players <- get_players_all_local()
    
    # Re-resolve identity from api_player_id before anything else: a
    # mapping filled in since the ingest ran takes effect here, across
    # all history, with no re-scrape.
    stats <- resolve_player_ids(stats, players)
    
    stats$player_id <- as.character(stats$player_id)
    unknown <- is.na(stats$player_id) |
      !stats$player_id %in% as.character(players$player_id)
    if (any(unknown)) {
      ids <- unique(stats$player_id[unknown])
      message(">>> [leaderboard] *** ", sum(unknown), " stat row(s) across ",
              length(ids), " unknown player_id(s) DROPPED — they score for ",
              "nobody. Check the players snapshot is current and the ",
              "api_player_id mapping is complete: ",
              paste(utils::head(ids, 8), collapse = ", "))
      stats <- stats[!unknown, , drop = FALSE]
    }
    if (nrow(stats) == 0) return(NULL)
    
    scored <- tryCatch(
      score_player_match_stats(stats, players),
      error = function(e) {
        message(">>> [leaderboard] *** SCORING FAILED — board renders ",
                "dashes *** : ", conditionMessage(e))
        NULL
      }
    )
    if (is.null(scored)) return(NULL)
    
    tryCatch(
      assign_gameweeks(scored),
      error = function(e) {
        message(">>> [leaderboard] *** GAMEWEEK ASSIGNMENT FAILED — board ",
                "renders dashes *** : ", conditionMessage(e))
        NULL
      }
    )
  })
  
  # Optimal-XI points per entry per gameweek. THIS is the contest: the
  # board's Pts column, the gameweek pills and the squad panel all read
  # from here, so there is exactly one definition of a score in the app.
  lb_gw_points <- reactive({
    scored <- lb_scored()
    picks  <- lb_picks()
    if (is.null(scored) || nrow(scored) == 0) return(NULL)
    if (is.null(picks)  || nrow(picks)  == 0) return(NULL)
    if (!"gameweek" %in% names(scored) || all(is.na(scored$gameweek))) {
      message(">>> [leaderboard] *** no scored row carries a gameweek — ",
              "bestball cannot run, board renders dashes ***")
      return(NULL)
    }
    tryCatch(
      bestball_gameweek_points(scored, picks,
                               players   = get_players_all_local(),
                               gw_period = lb_gw_period()),
      error = function(e) {
        message(">>> [leaderboard] *** BESTBALL FAILED — board renders ",
                "dashes *** : ", conditionMessage(e))
        NULL
      }
    )
  })
  
  # Gameweeks with any scored rows — the pills that light up. A gameweek
  # becomes selectable as soon as its first match is ingested, showing
  # points to date for that round rather than waiting for it to close.
  lb_available_gws <- reactive({
    gameweeks_with_stats(lb_scored())
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
    
    entries$points <- lb_entry_points(entries, lb_gw_points())
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
    
    notice <- if (nzchar(LB_NOTICE)) {
      div(class = "lb-status-notice", LB_NOTICE)
    }
    
    line <- if (nrow(rows) == 0 || all(is.na(rows$points))) {
      span("Scoring goes live once Gameweek 1 matches are in \u2014 points
            will appear here as they are scored.")
    } else {
      gws <- lb_available_gws()
      open_gw <- gws[!vapply(gws, gameweek_is_closed, logical(1))]
      span(
        paste0(
          "Best XI scoring. Gameweeks scored: ",
          if (length(gws) == 0) "\u2014" else paste(gws, collapse = ", "),
          ".",
          if (length(open_gw) > 0)
            paste0(" GW", paste(open_gw, collapse = ", "),
                   " still in progress \u2014 points to date only.")
          else "",
          " Scores may be provisional while match stats are collated."
        )
      )
    }
    
    tagList(line, notice)
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
    
    avail  <- lb_available_gws()
    closed <- avail[vapply(avail, gameweek_is_closed, logical(1))]
    
    # Default to the latest gameweek carrying stats; fall back to the
    # opening squad only when nothing has been scored yet. A stale selection
    # (a gameweek whose stats were withdrawn) drops back to the default
    # rather than rendering a pitch nobody can navigate away from.
    default <- if (length(avail) > 0) paste0("gw:", max(avail)) else "squad"
    chosen  <- lb_gw_view()
    active  <- if (is.null(chosen)) default else chosen
    
    sel_gw <- NA_integer_
    if (grepl("^gw:", active)) {
      g <- suppressWarnings(as.integer(sub("^gw:", "", active)))
      if (!is.na(g) && g %in% avail) {
        sel_gw <- g
      } else {
        active <- default
        if (grepl("^gw:", default)) sel_gw <- max(avail)
      }
    }
    
    selector <- pitch_gw_selector(
      input_id      = "lb_gw_select",
      active        = active,
      first_gw      = first_gw,
      last_gw       = last_gw,
      available_gws = avail,
      closed_gws    = closed
    )
    
    # --- The transfers view ---------------------------------------------
    # "transfers": the entry's rows from the transfers audit log, in
    # place of the pitch. Cached read (5-min TTL); the tab not existing
    # yet renders as the empty state, never an error.
    if (identical(active, "transfers")) {
      tr_all <- tryCatch(get_transfers_cached(), error = function(e) {
        message(">>> [leaderboard] get_transfers_cached failed: ",
                conditionMessage(e))
        NULL
      })
      tr_entry <- if (is.null(tr_all) || nrow(tr_all) == 0) NULL else {
        tr_all |> filter(as.character(entry_id) == as.character(eid))
      }
      return(tagList(
        panel_header,
        selector,
        lb_transfers_panel(tr_entry, get_players_all_local(), club_name)
      ))
    }
    
    # --- The squad view -------------------------------------------------
    # "squad": all 15 in their bands with season totals, no XI split.
    # "gw:<n>": the optimal XI in the bands, everyone else on the bench,
    #           each chip carrying that gameweek's points.
    #
    # The pitch is drawn from the roster held FOR THE SELECTED GAMEWEEK.
    # `picks` (above) is the roster held now, which is right for the season
    # view and the stats list, but a GW1/GW2 pitch on an entry that has
    # since been auto-subbed must show the period-1 fifteen — otherwise the
    # outgoing player's XI chip has nothing to land on. Falls back to the
    # current roster only if the period cannot be resolved.
    squad_base <- picks |>
      mutate(pid = as.character(player_id)) |>
      select(pid, player_id, name, team, position)
    if (!is.na(sel_gw)) {
      gw_roster <- lb_period_roster(eid, sel_gw)
      if (!is.null(gw_roster)) squad_base <- gw_roster
      squad_base <- lb_historic_clubs(squad_base, lb_scored(), sel_gw)
    }
    
    if (is.na(sel_gw)) {
      squad <- squad_base
      squad$points <- as.numeric(stats$pts)
      squad$in_xi  <- NA
      summary_strip <- NULL
      gw_note <- NULL
    } else {
      xi <- tryCatch(
        bestball_entry_xi(lb_scored(), lb_picks(), eid, gw = sel_gw,
                          players   = get_players_all_local(),
                          gw_period = lb_gw_period()),
        error = function(e) {
          message(">>> [leaderboard] bestball_entry_xi failed: ",
                  conditionMessage(e))
          NULL
        }
      )
      if (is.null(xi)) {
        squad <- squad_base
        squad$points <- NA_real_
        squad$in_xi  <- NA
        summary_strip <- pitch_gw_summary("Gameweek points unavailable")
        gw_note <- NULL
      } else {
        xi$pid <- as.character(xi$player_id)
        squad <- squad_base |>
          left_join(xi |> select(pid, points, in_xi), by = "pid")
        squad$points <- ifelse(is.na(squad$points), 0, squad$points)
        squad$in_xi  <- ifelse(is.na(squad$in_xi), FALSE, squad$in_xi)
        
        is_closed <- sel_gw %in% closed
        n_played  <- length(unique(
          lb_scored()$match_id[!is.na(lb_scored()$gameweek) &
                                 lb_scored()$gameweek == sel_gw]))
        status <- if (is_closed) {
          paste0("GW", sel_gw, " · final")
        } else {
          paste0("GW", sel_gw, " · in progress · ", n_played,
                 " of 10 matches scored")
        }
        summary_strip <- pitch_gw_summary(
          status,
          xi_points    = sum(squad$points[squad$in_xi]),
          bench_points = sum(squad$points[!squad$in_xi])
        )
        
        # Only while the gameweek is open: most of a squad sits on 0 early
        # on, so the XI/bench split is decided by salary rather than by
        # performance. Saying so stops it reading as the app choosing badly.
        gw_note <- if (!is_closed) {
          div(class = "lb-gw-note",
              "Bench players don't score. While a gameweek is in progress
               most of a squad is still on zero, so the split falls to
               salary — the total is the same either way.")
        }
      }
    }
    
    # Flames mark the league's top five at each position for the selected
    # gameweek — league-wide, not within the entry, so it means "one of the
    # best returns anywhere this week" rather than "your best player".
    hot_ids <- if (is.na(sel_gw)) character(0) else {
      tryCatch(bestball_hot_players(lb_scored(), sel_gw),
               error = function(e) character(0))
    }
    
    pitch_ui <- if (is.null(lb_gw_points())) {
      pitch_readonly(picks, "No picks found for this entry.")
    } else {
      pitch_squad(squad, "No picks found for this entry.", hot_ids = hot_ids)
    }
    
    # The per-player Pts in the list below are each player's OWN total across
    # every appearance. They deliberately do not sum to the entry total,
    # which counts only each gameweek's best XI — without this line the
    # panel reads as an arithmetic error to anyone who adds up the column.
    totals_note <- if (!is.null(lb_gw_points())) {
      div(class = "lb-roster-note",
          "Season totals for all 15. The entry total counts only the best XI
           in each gameweek, so this column will not add up to it.")
    }
    
    tagList(
      panel_header,
      selector,
      summary_strip,
      pitch_ui,
      gw_note,
      totals_note,
      div(class = "lb-roster-list", stats_header, row_uis)
    )
  })
  
  # Ownership table (below the roster panel) --------------------------
  
  lb_own_limit <- reactiveVal(10L)
  
  # Ownership across the whole contest: how many entries hold each
  # player, ranked. Built from the same snapshots as the board (or the
  # same live fallbacks), so it costs nothing extra.
  #
  # PERIOD-AWARE: roster_picks holds a full roster per period, so an entry
  # touched by a club-clash auto-sub or a window transfer has 30 rows.
  # Ownership is "who holds him NOW", so each entry is reduced to its
  # highest present period before counting — otherwise every corrected
  # entry double-counts and percentages exceed 100.
  lb_ownership_data <- reactive({
    rows <- lb_data()
    if (is.null(rows) || nrow(rows) == 0) return(NULL)
    
    picks <- lb_picks()
    if (is.null(picks) || nrow(picks) == 0) return(NULL)
    
    if ("period" %in% names(picks)) {
      pp <- suppressWarnings(as.integer(picks$period))
      picks <- picks |>
        mutate(.period = ifelse(is.na(pp), 1L, pp)) |>
        group_by(entry_id) |>
        filter(.period == max(.period)) |>
        ungroup() |>
        select(-.period)
    }
    message("    [leaderboard] ownership from ", nrow(picks),
            " current-roster picks across ",
            length(unique(picks$entry_id)), " entries")
    
    players <- get_players_all_local()
    picks |>
      mutate(player_id = as.integer(player_id)) |>
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
      lb_gw_view(NULL)
    }
  })
}