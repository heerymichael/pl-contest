# Lineup creator + reusable lineup picker.
#
# Modes:
#   - "new"  : standalone Build a Lineup view. Saves via create_entry +
#              save_roster_picks. Uses localStorage. Bounces to My Lineups
#              on success.
#   - "edit" : embedded inside My Lineups. Saves via update_entry_picks.
#              No localStorage. Closes editor in place on success.
#
# Lock-time:
#   - Picker UI elements respect is_locked_reactive (pool rows not
#     clickable, Remove/Undo/Reset hidden, Submit/Save replaced with
#     "Contest locked").
#   - Submit/save handlers fresh-read is_locked() as a defence against
#     stale UI.

message(">>> SOURCED R/lineup_creator.R at ", format(Sys.time()))

library(shiny)
library(dplyr)
library(tibble)

# --- Shared input id helpers ------------------------------------------

picker_id <- function(mode, name) paste0(mode, "_", name)

# Player pool pagination — rows rendered per "page". 30 covers a full
# national squad (~26 players) or several position groups at once; the
# Show-more footer in the pool reveals the next page.
POOL_PAGE_SIZE <- 30L

# --- Picker UI (shared) ------------------------------------------------

lineup_picker_ui <- function(mode) {
  div(
    id    = picker_id(mode, "root"),
    class = "lineup-picker picker-mobile-lineup",
    
    div(
      class = "entry-name-row",
      textInput(picker_id(mode, "entry_name"), label = NULL,
                placeholder = "Lineup name (optional)",
                width = "100%")
    ),
    
    fluidRow(
      column(
        width = 7,
        div(
          class = "panel-card",
          actionButton(
            picker_id(mode, "pool_back"),
            label = HTML("&larr; Back to lineup"),
            class = "btn btn-default pool-back-btn"
          ),
          div(
            class = "panel-card-header",
            h3("Player Pool"),
            div(
              class = "pool-toggle-inline",
              checkboxInput(picker_id(mode, "hide_unavailable"),
                            "Hide unavailable players",
                            value = TRUE)
            )
          ),
          fluidRow(
            column(4, uiOutput(picker_id(mode, "filter_team_ui"))),
            column(4, selectInput(picker_id(mode, "filter_position"), "Position",
                                  choices = c("All positions" = "ALL",
                                              "GK"  = "GK",
                                              "DEF" = "DEF",
                                              "MID" = "MID",
                                              "FWD" = "FWD"))),
            column(4, textInput(picker_id(mode, "filter_name"), "Name search",
                                placeholder = "type to search…"))
          ),
          div(
            class = "player-list-scroll",
            wc_spinner(uiOutput(picker_id(mode, "player_pool_ui")))
          )
        )
      ),
      column(
        width = 5,
        div(
          class = "panel-card panel-card-dark",
          h3("Your Lineup"),
          wc_spinner(uiOutput(picker_id(mode, "roster_ui"))),
          uiOutput(picker_id(mode, "picker_actions")),
          div(
            class = "submit-row",
            uiOutput(picker_id(mode, "submit_button_ui"))
          )
        )
      )
    )
  )
}

# --- Standalone creator view (mode = "new") ---------------------------

lineup_creator_view <- function() {
  div(
    class = "view",
    div(
      class = "lineup-header",
      div(
        div(class = "lineup-eyebrow", "New lineup"),
        h2("Build a Lineup")
      )
    ),
    lineup_picker_ui("new")
  )
}

# --- Editor area (mode = "edit") --------------------------------------

lineup_editor_ui <- function() {
  div(
    class = "lineup-editor",
    lineup_picker_ui("edit")
  )
}

# --- Picker server logic (shared) -------------------------------------

lineup_picker_server_logic <- function(input, output, session,
                                       mode, current_user, view_state,
                                       initial_picks      = reactive(integer(0)),
                                       editing_entry_id   = reactive(NULL),
                                       is_locked_reactive = reactive(FALSE),
                                       on_dirty_change    = function(is_dirty) {},
                                       on_save_success    = function() {}) {
  
  message(">>> ENTER lineup_picker_server_logic mode=", mode)
  
  iid <- function(name) picker_id(mode, name)
  
  # State -----------------------------------------------------------
  
  selected_ids   <- reactiveVal(integer(0))
  pending_submit <- reactiveVal(FALSE)
  restored       <- reactiveVal(mode != "new")
  baseline_ids   <- reactiveVal(integer(0))
  
  # Post-bind re-render trigger. The pre-warmed outputs below
  # (suspendWhenHidden = FALSE) compute during the very first flush,
  # before the client has inserted the page UI — their initial values
  # arrive with no bound element to land on, so the panels sit on
  # their spinners. Bumping ui_ready after that first flush forces one
  # recompute whose values arrive after binding.
  ui_ready <- reactiveVal(0L)
  session$onFlushed(function() {
    message("    [", mode, "] first flush complete — bumping ui_ready")
    ui_ready(isolate(ui_ready()) + 1L)
  }, once = TRUE)
  
  # Mobile-only state: drives slot-click → pool → pick → back-to-lineup
  # flow. Set regardless of viewport; desktop CSS ignores it.
  mobile_view         <- reactiveVal("lineup")   # "lineup" | "pool"
  replacing_player_id <- reactiveVal(NULL)       # set on Replace, consumed on next pick
  
  # Pool pagination: how many rows the pool currently renders. Reset to
  # one page whenever a filter, search, or the availability toggle
  # changes; grown by the Show-more footer.
  pool_limit <- reactiveVal(POOL_PAGE_SIZE)
  
  # Edit-mode dormancy: the editor's pre-warmed outputs compute during
  # the first flush even when no lineup is open for editing. When
  # dormant, each returns NULL cheaply instead of building full UI.
  # editing_entry_id() is the reactive dependency that wakes them all
  # when a lineup is opened.
  editor_dormant <- reactive({
    mode == "edit" && is.null(editing_entry_id())
  })
  
  observe({
    message("    [", mode, "] mobile_view -> ", mobile_view(),
            if (!is.null(replacing_player_id()))
              paste0(" (replacing player_id=", replacing_player_id(), ")")
            else "")
  })
  
  # Push mobile-view class to client. JS handler swaps the
  # picker-mobile-{lineup|pool} class on the picker root without
  # re-rendering — no flicker on the roster panel.
  observe({
    view <- mobile_view()
    session$sendCustomMessage(
      "wc_set_picker_mobile_view",
      list(id = iid("root"), view = view)
    )
  })
  
  # Cached session-scope data --------------------------------------
  
  players_data <- reactive({
    out <- get_players_cached()
    message("    [", mode, "] players_data — ", nrow(out), " rows")
    out
  })
  teams_data <- reactive({
    out <- get_teams_cached()
    message("    [", mode, "] teams_data — ", nrow(out), " rows")
    out
  })
  config_data <- reactive({
    get_config_cached()
  })
  
  # Team filter dropdown with flags via selectizeInput custom render.
  # Each choice label is HTML (flag <img> + country name); the render
  # functions output that HTML directly without escaping.
  output[[iid("filter_team_ui")]] <- renderUI({
    if (editor_dormant()) return(NULL)   # edit picker idle — skip the build
    ui_ready()   # re-render once after client-side binding
    teams <- teams_data()
    
    labels <- c(
      "All teams",
      vapply(seq_len(nrow(teams)), function(i) {
        flag_html_label(teams$short_code[i], teams$name[i])
      }, character(1))
    )
    values <- c("ALL", teams$short_code)
    
    selectizeInput(
      iid("filter_team"), "Team",
      choices  = setNames(values, labels),
      selected = "ALL",
      options  = list(
        render = I("{
          option: function(item, escape) { return '<div class=\"flag-option\">' + item.label + '</div>'; },
          item:   function(item, escape) { return '<div class=\"flag-option\">' + item.label + '</div>'; }
        }"),
        # Clear on open so the user can start typing immediately; if they
        # close without picking, restore whatever was selected before.
        onDropdownOpen = I("function() {
          console.log('[wc] team dropdown opened; value=', this.getValue());
          this._previous = this.getValue();
          if (this._previous) {
            this.removeItem(this._previous, true);
          }
        }"),
        onDropdownClose = I("function() {
          if (!this.getValue() && this._previous) {
            this.setValue(this._previous, false);
          }
        }")
      )
    )
  })
  
  # Initial picks loading (edit mode) -------------------------------
  
  if (mode == "edit") {
    observeEvent(initial_picks(), {
      ids <- initial_picks()
      message(">>> [edit] loading initial_picks: ", length(ids), " ids")
      selected_ids(as.integer(ids))
      baseline_ids(as.integer(ids))
      updateTextInput(session, iid("entry_name"), value = "")
      # Reset mobile picker state on each lineup-load so a stale pool
      # view doesn't carry over from a previously-edited lineup.
      mobile_view("lineup")
      replacing_player_id(NULL)
    }, ignoreNULL = FALSE)
  }
  
  # localStorage (new mode only) ------------------------------------
  
  if (mode == "new") {
    # Reset mobile picker state whenever the user lands on Build a
    # Lineup, so a stale pool view doesn't carry over after navigating
    # away and back.
    observeEvent(view_state(), {
      if (view_state() == "lineup") {
        message(">>> [new] view_state -> lineup; resetting mobile_view")
        mobile_view("lineup")
        replacing_player_id(NULL)
      }
    })
    
    session$onFlushed(function() {
      session$sendCustomMessage(
        "wc_load_ls",
        list(key = "wc_lineup_state", input_id = "ls_restored")
      )
    }, once = TRUE)
    
    # ignoreInit = TRUE is load-bearing here: without it, this observer
    # fires immediately on first flush with input$ls_restored still NULL
    # (ignoreNULL = FALSE doesn't skip the init run), and once = TRUE
    # then destroys it before the wc_load_ls round trip completes — so
    # saved picks were never restored. The JS handler always calls
    # setInputValue with {priority: 'event'} (null payload when no saved
    # state), so this observer is guaranteed exactly one real trigger.
    observeEvent(input$ls_restored, {
      data <- input$ls_restored
      if (!is.null(data)) {
        if (!is.null(data$picks) && length(data$picks) > 0) {
          message("    [new] restoring ", length(data$picks),
                  " picks from localStorage")
          selected_ids(as.integer(unlist(data$picks)))
        }
        if (!is.null(data$name) && nzchar(data$name)) {
          updateTextInput(session, iid("entry_name"), value = data$name)
        }
        if (!is.null(data$hide_unavailable)) {
          message("    [new] restoring hide_unavailable=", isTRUE(data$hide_unavailable))
          updateCheckboxInput(session, iid("hide_unavailable"),
                              value = isTRUE(data$hide_unavailable))
        }
      }
      restored(TRUE)
    }, once = TRUE, ignoreInit = TRUE, ignoreNULL = FALSE)
    
    entry_name_debounced <- debounce(reactive({ input[[iid("entry_name")]] }), 500)
    
    observe({
      req(restored())
      ids  <- selected_ids()
      name <- entry_name_debounced() %||% ""
      hide <- isTRUE(input[[iid("hide_unavailable")]])
      session$sendCustomMessage("wc_save_ls", list(
        key   = "wc_lineup_state",
        value = list(picks = as.list(ids), name = name, hide_unavailable = hide)
      ))
    })
  }
  
  # Dirty tracking (edit mode only) ---------------------------------
  
  if (mode == "edit") {
    is_dirty <- reactive({
      current  <- sort(selected_ids())
      baseline <- sort(baseline_ids())
      !identical(current, baseline)
    })
    
    observe({
      d <- is_dirty()
      on_dirty_change(d)
    })
  }
  
  # Derived state ---------------------------------------------------
  
  selected_players <- reactive({
    ids <- selected_ids()
    if (length(ids) == 0) {
      return(tibble(player_id = integer(0), name = character(0),
                    team = character(0), position = character(0)))
    }
    players_data() |>
      filter(player_id %in% ids) |>
      arrange(match(player_id, ids))
  })
  
  selection_state <- reactive({
    sp  <- selected_players()
    cfg <- config_data()
    
    pos_max <- c(
      GK  = cfg$max_gk  %||% 1,
      DEF = cfg$max_def %||% 5,
      MID = cfg$max_mid %||% 6,
      FWD = cfg$max_fwd %||% 4
    )
    
    counts <- c(GK = 0L, DEF = 0L, MID = 0L, FWD = 0L)
    if (nrow(sp) > 0) {
      tab <- table(sp$position)
      counts[names(tab)] <- as.integer(tab)
    }
    
    list(
      counts         = counts,
      total          = nrow(sp),
      teams_taken    = unique(sp$team),
      positions_full = names(counts)[counts >= pos_max[names(counts)]]
    )
  })
  
  filtered_pool <- reactive({
    pool <- players_data()
    
    ft <- input[[iid("filter_team")]]
    fp <- input[[iid("filter_position")]]
    fn <- input[[iid("filter_name")]]
    
    if (!is.null(ft) && nzchar(ft) && ft != "ALL") {
      pool <- pool |> filter(team == ft)
    }
    if (!is.null(fp) && fp != "ALL") {
      pool <- pool |> filter(position == fp)
    }
    if (!is.null(fn) && nzchar(fn)) {
      q <- tolower(fn)
      pool <- pool |> filter(grepl(q, tolower(name), fixed = TRUE))
    }
    
    # Sort by FanTeam price, highest first. Prices are sort-only —
    # never displayed in the UI. arrange() always places NA last, so
    # the handful of unpriced players sink to the bottom of any
    # filtered list. Name breaks ties within a price band.
    pool |> arrange(desc(fanteam_price), name)
  })
  
  # Single source of truth for roster validity. Returns the pick count,
  # the target, completeness, and a one-line message for the submit row
  # ("8 of 11 players selected — pick 3 more"). Only the *first* failed
  # rule is named — one actionable step at a time. The rule order
  # mirrors the original is_lineup_complete checks (total, position
  # min/max, one-per-nation), so complete == all rules pass.
  lineup_status <- reactive({
    state <- selection_state()
    cfg   <- config_data()
    sp    <- selected_players()
    
    target <- as.integer(cfg$roster_total %||% 11)
    pos_min <- c(
      GK  = as.integer(cfg$min_gk  %||% 1), DEF = as.integer(cfg$min_def %||% 3),
      MID = as.integer(cfg$min_mid %||% 3), FWD = as.integer(cfg$min_fwd %||% 1)
    )
    pos_max <- c(
      GK  = as.integer(cfg$max_gk  %||% 1), DEF = as.integer(cfg$max_def %||% 5),
      MID = as.integer(cfg$max_mid %||% 6), FWD = as.integer(cfg$max_fwd %||% 4)
    )
    
    counts <- state$counts
    total  <- state$total
    
    count_line <- sprintf("%d of %d players selected", total, target)
    
    problem <- NULL
    if (total < target) {
      n <- target - total
      problem <- sprintf("pick %d more", n)
    } else if (total > target) {
      problem <- sprintf("remove %d", total - target)
    } else if (any(counts < pos_min)) {
      p <- names(counts)[counts < pos_min][1]
      problem <- sprintf("need at least %d %s (you have %d)",
                         pos_min[[p]], p, counts[[p]])
    } else if (any(counts > pos_max)) {
      p <- names(counts)[counts > pos_max][1]
      problem <- sprintf("max %d %s allowed (you have %d)",
                         pos_max[[p]], p, counts[[p]])
    } else if (length(state$teams_taken) != total) {
      dup <- unique(sp$team[duplicated(sp$team)])
      problem <- sprintf("max 1 player per nation (%s picked more than once)",
                         paste(dup, collapse = ", "))
    }
    
    list(
      total    = total,
      target   = target,
      complete = is.null(problem),
      message  = if (is.null(problem)) count_line
      else paste0(count_line, " \u2014 ", problem)
    )
  })
  
  # UI renders ------------------------------------------------------
  
  output[[iid("player_pool_ui")]] <- renderUI({
    if (editor_dormant()) return(NULL)   # edit picker idle — skip the build
    ui_ready()   # re-render once after client-side binding
    locked        <- is_locked_reactive()
    hide_unavail  <- isTRUE(input[[iid("hide_unavailable")]])
    pool          <- filtered_pool()
    sel           <- selected_ids()
    rep_id        <- replacing_player_id()
    
    # In replace mode, the player being replaced shouldn't count toward
    # team-taken or position-full — otherwise valid same-team or
    # same-position swaps would be blocked.
    if (!is.null(rep_id)) {
      sel_for_state <- setdiff(sel, rep_id)
      sp_for_state  <- players_data() |> filter(player_id %in% sel_for_state)
      cfg           <- config_data()
      pos_max <- c(
        GK  = cfg$max_gk  %||% 1, DEF = cfg$max_def %||% 5,
        MID = cfg$max_mid %||% 6, FWD = cfg$max_fwd %||% 4
      )
      counts <- c(GK = 0L, DEF = 0L, MID = 0L, FWD = 0L)
      if (nrow(sp_for_state) > 0) {
        tab <- table(sp_for_state$position)
        counts[names(tab)] <- as.integer(tab)
      }
      state <- list(
        teams_taken    = unique(sp_for_state$team),
        positions_full = names(counts)[counts >= pos_max[names(counts)]]
      )
    } else {
      state <- selection_state()
    }
    
    # Toggle: when not locked, drop rows that are unavailable for reasons
    # other than the lock itself. When locked, every row would be disabled
    # so the toggle is inert (otherwise the pool empties entirely).
    toggle_emptied <- FALSE
    if (hide_unavail && !locked && nrow(pool) > 0) {
      before_n <- nrow(pool)
      pool <- pool |> filter(
        !player_id %in% sel,
        !team      %in% state$teams_taken,
        !position  %in% state$positions_full
      )
      toggle_emptied <- nrow(pool) == 0 && before_n > 0
    }
    
    if (nrow(pool) == 0) {
      msg <- if (toggle_emptied) {
        "All matching players are unavailable. Untick \"Hide unavailable players\" to see them."
      } else {
        "No players match these filters."
      }
      return(div(class = "empty-state", p(msg)))
    }
    
    pick_input_id <- iid("lineup_pick_player")
    
    # Pagination slice — applied after *all* filtering (team, position,
    # name, hide-unavailable) so a rendered page is never silently
    # short. The footer below reveals the next page.
    total_n <- nrow(pool)
    shown_n <- min(pool_limit(), total_n)
    pool    <- pool |> slice_head(n = shown_n)
    
    rows <- lapply(seq_len(nrow(pool)), function(i) {
      pl <- pool[i, ]
      
      is_picked     <- pl$player_id %in% sel
      is_team_taken <- !is_picked && pl$team     %in% state$teams_taken
      is_pos_full   <- !is_picked && pl$position %in% state$positions_full
      disabled      <- locked || is_picked || is_team_taken || is_pos_full
      
      reason <- if (locked)         "Contest locked"
      else if (is_picked)           "Already in your lineup"
      else if (is_team_taken)       "Another player from this team is already picked"
      else if (is_pos_full)         "This position group is full"
      else                          NULL
      
      short_label <- if (locked)    "Locked"
      else if (is_picked)           "In lineup"
      else if (is_team_taken)       "Team picked"
      else if (is_pos_full)         "Position full"
      else                          NULL
      
      row_class <- paste(
        "player-row",
        if (disabled) "player-row-disabled" else "player-row-active"
      )
      
      label_class <- paste(
        "player-disabled-label",
        if (is_picked) "player-disabled-label-active"
      )
      
      onclick_js <- if (!disabled) {
        sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'});",
                pick_input_id, pl$player_id)
      } else NULL
      
      tags$div(
        class   = row_class,
        title   = reason %||% "",
        onclick = onclick_js,
        tags$div(
          class = "player-row-text",
          tags$div(class = "player-name", pl$name),
          tags$div(class = "player-meta",
                   flag_tag(pl$team, size = "sm"),
                   pl$team, " · ", pl$position)
        ),
        if (!is.null(short_label))
          tags$div(
            class = "player-row-right",
            tags$span(class = label_class, short_label)
          )
      )
    })
    # "Showing N of M · Show 30 more" footer — only when rows remain.
    # The button shrinks to the actual remainder on the last page.
    footer <- if (total_n > shown_n) {
      remaining    <- total_n - shown_n
      show_more_id <- iid("pool_show_more")
      tags$div(
        class = "pool-pagination-row",
        tags$span(
          class = "pool-pagination-count",
          sprintf("Showing %d of %s players",
                  shown_n, format(total_n, big.mark = ","))
        ),
        tags$button(
          class   = "btn btn-default btn-xs pool-show-more-btn",
          type    = "button",
          onclick = sprintf(
            "Shiny.setInputValue('%s', Date.now(), {priority: 'event'});",
            show_more_id),
          sprintf("Show %d more", min(POOL_PAGE_SIZE, remaining))
        )
      )
    } else NULL
    
    do.call(tagList, c(rows, list(footer)))
  })
  
  output[[iid("roster_ui")]] <- renderUI({
    if (editor_dormant()) return(NULL)   # edit picker idle — skip the build
    ui_ready()   # re-render once after client-side binding
    locked <- is_locked_reactive()
    sp     <- selected_players()
    cfg    <- config_data()
    
    pos_meta <- list(
      GK  = list(label = "GOALKEEPER",  min = as.integer(cfg$min_gk  %||% 1), max = as.integer(cfg$max_gk  %||% 1)),
      DEF = list(label = "DEFENDERS",   min = as.integer(cfg$min_def %||% 3), max = as.integer(cfg$max_def %||% 5)),
      MID = list(label = "MIDFIELDERS", min = as.integer(cfg$min_mid %||% 3), max = as.integer(cfg$max_mid %||% 6)),
      FWD = list(label = "FORWARDS",    min = as.integer(cfg$min_fwd %||% 1), max = as.integer(cfg$max_fwd %||% 4))
    )
    target_total   <- as.integer(cfg$roster_total %||% 11)
    total_required <- sum(vapply(pos_meta, function(m) m$min, integer(1)))
    total_flex     <- target_total - total_required
    
    filled_by_pos <- c(GK = 0L, DEF = 0L, MID = 0L, FWD = 0L)
    if (nrow(sp) > 0) {
      tab <- table(sp$position)
      filled_by_pos[names(tab)] <- as.integer(tab)
    }
    
    flex_used <- sum(vapply(names(pos_meta), function(p) {
      max(0L, filled_by_pos[[p]] - pos_meta[[p]]$min)
    }, integer(1)))
    flex_remaining <- max(0L, total_flex - flex_used)
    
    remove_input_id  <- iid("lineup_remove_player")
    replace_input_id <- iid("lineup_replace_player")
    slot_input_id    <- iid("slot_click")
    
    groups <- lapply(names(pos_meta), function(p) {
      meta            <- pos_meta[[p]]
      filled_n        <- filled_by_pos[[p]]
      req_remaining   <- max(0L, meta$min - filled_n)
      cap_remaining   <- max(0L, meta$max - filled_n)
      opt_here_max    <- max(0L, cap_remaining - req_remaining)
      opt_visible     <- if (locked) 0L else min(opt_here_max, flex_remaining)
      
      filled_rows <- if (filled_n > 0) {
        pos_players <- sp |> filter(position == p) |> arrange(name)
        lapply(seq_len(nrow(pos_players)), function(i) {
          pl <- pos_players[i, ]
          tags$div(
            class = "roster-row roster-row-filled",
            tags$span(
              class = "roster-row-text",
              tags$span(class = "roster-position", pl$position),
              tags$span(class = "roster-name", pl$name),
              tags$span(class = "roster-team",
                        " · ",
                        flag_tag(pl$team, size = "sm"),
                        pl$team)
            ),
            if (!locked) tags$div(
              class = "roster-row-actions",
              tags$button(
                class   = "btn btn-default btn-xs replace-btn",
                type    = "button",
                onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'});",
                                  replace_input_id, pl$player_id),
                "Replace"
              ),
              tags$button(
                class   = "btn btn-default btn-xs remove-btn",
                type    = "button",
                onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'});",
                                  remove_input_id, pl$player_id),
                "Remove"
              )
            )
          )
        })
      } else list()
      
      required_empty_rows <- if (req_remaining > 0 && !locked) {
        lapply(seq_len(req_remaining), function(i) {
          tags$div(
            class   = "roster-row roster-row-empty roster-row-empty-required",
            onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'});",
                              slot_input_id, p),
            tags$span(
              class = "roster-row-text",
              tags$span(class = "roster-position", p),
              tags$span(class = "roster-slot-placeholder", "Empty")
            )
          )
        })
      } else list()
      
      optional_empty_rows <- if (opt_visible > 0 && !locked) {
        lapply(seq_len(opt_visible), function(i) {
          tags$div(
            class   = "roster-row roster-row-empty roster-row-empty-optional",
            onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'});",
                              slot_input_id, p),
            tags$span(
              class = "roster-row-text",
              tags$span(class = "roster-position", p),
              tags$span(class = "roster-slot-placeholder", "Empty")
            ),
            tags$span(class = "roster-optional-pill", "Optional")
          )
        })
      } else list()
      
      if (length(filled_rows) == 0 &&
          length(required_empty_rows) == 0 &&
          length(optional_empty_rows) == 0) return(NULL)
      
      tags$div(
        class = "roster-group",
        tags$div(class = "roster-group-header", meta$label),
        filled_rows,
        required_empty_rows,
        optional_empty_rows
      )
    })
    
    groups <- Filter(Negate(is.null), groups)
    
    if (length(groups) == 0) {
      return(div(class = "empty-state",
                 p("This lineup has no players.")))
    }
    
    do.call(tagList, groups)
  })
  
  # Undo/Reset row: hidden entirely when locked
  output[[iid("picker_actions")]] <- renderUI({
    if (editor_dormant()) return(NULL)   # edit picker idle — skip the build
    ui_ready()   # re-render once after client-side binding
    locked <- is_locked_reactive()
    if (locked) return(NULL)
    div(
      class = "lineup-picker-actions",
      actionButton(iid("lineup_undo"),  "Undo",
                   class = "btn btn-default"),
      actionButton(iid("lineup_reset"), "Reset",
                   class = "btn btn-default")
    )
  })
  
  output[[iid("submit_button_ui")]] <- renderUI({
    if (editor_dormant()) return(NULL)   # edit picker idle — skip the build
    ui_ready()   # re-render once after client-side binding
    locked    <- is_locked_reactive()
    status    <- lineup_status()
    complete  <- status$complete
    is_authed <- !is.null(current_user())
    
    label <- if (locked) {
      "Contest locked"
    } else if (mode == "edit") {
      "Save changes"
    } else if (is_authed) {
      "Submit lineup"
    } else {
      "Sign up to submit"
    }
    
    # Status line: always shows the pick count while unlocked, plus the
    # first failed rule when incomplete — so a greyed-out button is
    # never unexplained.
    status_div <- if (!locked) {
      div(
        class = paste(
          "submit-status",
          if (complete) "submit-status-complete" else "submit-status-incomplete"
        ),
        status$message
      )
    } else NULL
    
    btn <- if (complete && !locked) {
      actionButton(iid("submit_lineup"), label,
                   class = "btn btn-primary btn-block")
    } else {
      tags$button(class = "btn btn-primary btn-block",
                  disabled = "disabled",
                  type     = "button",
                  label)
    }
    
    tagList(status_div, btn)
  })
  
  # Mobile: the picker hides one panel at a time via display:none
  # (picker-mobile-* classes toggled by JS). Shiny suspends hidden
  # outputs by default and only re-checks visibility on events like a
  # window resize — a class toggle doesn't fire one, so a hidden panel's
  # outputs would never render. Pre-warm every output that can be
  # hidden by the mobile view toggle. Desktop is unaffected (both
  # panels always visible).
  for (.out in c("filter_team_ui", "player_pool_ui",
                 "roster_ui", "picker_actions", "submit_button_ui")) {
    outputOptions(output, iid(.out), suspendWhenHidden = FALSE)
  }
  
  # Interactions ----------------------------------------------------
  
  # Pool pagination: Show-more grows the rendered window by one page;
  # any change to the team filter, position filter, name search, or
  # availability toggle resets it to the first page. The reset also
  # covers the mobile slot-tap and Replace flows automatically, since
  # both work by setting the position filter.
  observeEvent(input[[iid("pool_show_more")]], {
    pool_limit(pool_limit() + POOL_PAGE_SIZE)
    message("    [", mode, "] pool_limit -> ", pool_limit())
  })
  
  observeEvent(list(input[[iid("filter_team")]],
                    input[[iid("filter_position")]],
                    input[[iid("filter_name")]],
                    input[[iid("hide_unavailable")]]), {
                      pool_limit(POOL_PAGE_SIZE)
                    }, ignoreInit = TRUE)
  
  observeEvent(input[[iid("lineup_pick_player")]], {
    pid <- as.integer(input[[iid("lineup_pick_player")]])
    if (is.na(pid)) return()
    sel <- selected_ids()
    if (pid %in% sel) return()
    pl <- players_data() |> filter(player_id == pid)
    if (nrow(pl) == 0) return()
    
    # If we're in replace mode, validate against state *excluding* the
    # player being replaced — otherwise their team/position would block
    # a valid swap. Then swap in-place: remove the old, append the new.
    rep_id <- replacing_player_id()
    if (!is.null(rep_id)) {
      message("    [", mode, "] pick in replace mode: ", rep_id, " -> ", pid)
      sel_excl <- setdiff(sel, rep_id)
      sp_excl  <- players_data() |> filter(player_id %in% sel_excl)
      teams_taken_excl    <- unique(sp_excl$team)
      # Position cap check: count existing minus the one we're swapping out.
      # Same-position swap is fine; cross-position swap needs the cap check
      # to use the new player's position against the post-swap counts.
      if (pl$team %in% teams_taken_excl) {
        message("    [", mode, "] replace blocked: team taken by another pick")
        return()
      }
      cfg <- config_data()
      pos_max <- c(
        GK  = cfg$max_gk  %||% 1, DEF = cfg$max_def %||% 5,
        MID = cfg$max_mid %||% 6, FWD = cfg$max_fwd %||% 4
      )
      counts_excl <- c(GK = 0L, DEF = 0L, MID = 0L, FWD = 0L)
      if (nrow(sp_excl) > 0) {
        tab <- table(sp_excl$position)
        counts_excl[names(tab)] <- as.integer(tab)
      }
      if (counts_excl[[pl$position]] >= pos_max[[pl$position]]) {
        message("    [", mode, "] replace blocked: position group full")
        return()
      }
      selected_ids(c(sel_excl, pid))
      replacing_player_id(NULL)
      mobile_view("lineup")
      return()
    }
    
    state <- selection_state()
    if (pl$team     %in% state$teams_taken)    return()
    if (pl$position %in% state$positions_full) return()
    selected_ids(c(sel, pid))
    mobile_view("lineup")
  }, ignoreInit = TRUE)
  
  observeEvent(input[[iid("lineup_remove_player")]], {
    pid <- as.integer(input[[iid("lineup_remove_player")]])
    if (is.na(pid)) return()
    selected_ids(setdiff(selected_ids(), pid))
  }, ignoreInit = TRUE)
  
  # Mobile slot-click on an empty slot: pre-filter pool by position
  # and flip the mobile view to "pool". Desktop CSS ignores the view.
  observeEvent(input[[iid("slot_click")]], {
    pos <- input[[iid("slot_click")]]
    if (is.null(pos) || !nzchar(pos)) return()
    message(">>> [", mode, "] slot_click pos=", pos)
    updateSelectInput(session, iid("filter_position"), selected = pos)
    replacing_player_id(NULL)
    mobile_view("pool")
  }, ignoreInit = TRUE)
  
  # Mobile Replace button on a filled row: stash the player being
  # replaced, pre-filter pool by *their* position, flip to pool.
  observeEvent(input[[iid("lineup_replace_player")]], {
    pid <- as.integer(input[[iid("lineup_replace_player")]])
    if (is.na(pid)) return()
    pl <- players_data() |> filter(player_id == pid)
    if (nrow(pl) == 0) return()
    message(">>> [", mode, "] replace requested for player_id=", pid,
            " pos=", pl$position)
    replacing_player_id(pid)
    updateSelectInput(session, iid("filter_position"), selected = pl$position)
    mobile_view("pool")
  }, ignoreInit = TRUE)
  
  # Mobile Back-to-lineup button on the pool view.
  observeEvent(input[[iid("pool_back")]], {
    message(">>> [", mode, "] pool_back")
    replacing_player_id(NULL)
    mobile_view("lineup")
  }, ignoreInit = TRUE)
  
  observeEvent(input[[iid("lineup_undo")]], {
    sel <- selected_ids()
    if (length(sel) == 0) return()
    selected_ids(sel[-length(sel)])
  })
  
  observeEvent(input[[iid("lineup_reset")]], {
    if (length(selected_ids()) == 0) return()
    showModal(modalDialog(
      title = "Reset lineup?",
      "This will remove all selected players.",
      footer = tagList(
        modalButton("Cancel"),
        actionButton(iid("lineup_reset_confirm"), "Reset",
                     class = "btn btn-danger")
      ),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input[[iid("lineup_reset_confirm")]], {
    if (mode == "edit") {
      selected_ids(baseline_ids())
    } else {
      selected_ids(integer(0))
      updateTextInput(session, iid("entry_name"), value = "")
      session$sendCustomMessage("wc_clear_ls", list(key = "wc_lineup_state"))
    }
    removeModal()
  })
  
  # Submit + save ---------------------------------------------------
  
  resolve_entry_name <- function(raw_name, user_email) {
    existing <- get_entries_for_user(user_email)
    existing_names <- if (nrow(existing) == 0) character(0) else existing$entry_name
    
    name <- trimws(raw_name %||% "")
    
    if (nzchar(name)) {
      if (nchar(name) > 40) {
        return(list(ok = FALSE,
                    error = "Lineup name must be 40 characters or fewer."))
      }
      if (name %in% existing_names) {
        return(list(ok = FALSE,
                    error = paste0("You already have an entry called \"", name, "\".")))
      }
      return(list(ok = TRUE, name = name))
    }
    
    matches  <- regmatches(existing_names, regexpr("^Lineup \\d+$", existing_names))
    nums_str <- sub("^Lineup ", "", matches)
    used_nums <- suppressWarnings(as.integer(nums_str))
    used_nums <- used_nums[!is.na(used_nums)]
    next_n <- if (length(used_nums) == 0) 1L else max(used_nums) + 1L
    list(ok = TRUE, name = paste("Lineup", next_n))
  }
  
  open_submit_confirm <- function(resolved_name) {
    session$userData$pending_entry_name <- resolved_name
    showModal(modalDialog(
      title = "Submit this lineup?",
      tagList(
        p(tags$strong("Lineup name: "), resolved_name),
        p("Once submitted, your lineup is final until the contest lock time. ",
          "You can edit it any time before lock from My Lineups.")
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton(iid("confirm_submit_lineup"), "Submit lineup",
                     class = "btn btn-primary")
      ),
      easyClose = TRUE
    ))
  }
  
  open_save_confirm <- function() {
    showModal(modalDialog(
      title = "Save changes to this lineup?",
      "Your existing picks will be replaced with the new selection.",
      footer = tagList(
        modalButton("Cancel"),
        actionButton(iid("confirm_save_lineup"), "Save changes",
                     class = "btn btn-primary")
      ),
      easyClose = TRUE
    ))
  }
  
  do_submit_flow <- function() {
    user <- current_user()
    if (is.null(user)) return()
    
    cfg <- config_data()
    max_entries <- cfg$max_entries_per_user %||% 10
    n_existing <- count_entries_for_user(user$email)
    if (n_existing >= max_entries) {
      showNotification(
        paste0("You've reached the maximum of ", max_entries,
               " entries. Delete one before creating another."),
        type = "error", duration = 8
      )
      return()
    }
    
    resolved <- resolve_entry_name(input[[iid("entry_name")]], user$email)
    if (!resolved$ok) {
      showNotification(resolved$error, type = "error", duration = 8)
      return()
    }
    
    open_submit_confirm(resolved$name)
  }
  
  observeEvent(input[[iid("submit_lineup")]], {
    message(">>> OBSERVE ", iid("submit_lineup"))
    
    if (is_locked()) {
      message("    !! submit blocked by fresh is_locked()")
      showNotification("Contest is locked. No further submissions or changes.",
                       type = "error", duration = 8)
      return()
    }
    
    if (mode == "edit") {
      open_save_confirm()
      return()
    }
    
    if (is.null(current_user())) {
      pending_submit(TRUE)
      show_signup_modal()
      return()
    }
    do_submit_flow()
  })
  
  if (mode == "new") {
    observeEvent(current_user(), {
      if (pending_submit() && !is.null(current_user())) {
        message(">>> [new] auto-fire submit after auth")
        pending_submit(FALSE)
        do_submit_flow()
      }
    }, ignoreInit = TRUE, ignoreNULL = FALSE)
  }
  
  observeEvent(input[[iid("confirm_submit_lineup")]], {
    message(">>> OBSERVE ", iid("confirm_submit_lineup"))
    removeModal()
    
    if (is_locked()) {
      message("    !! confirm blocked by fresh is_locked()")
      showNotification("Contest is locked. No further submissions or changes.",
                       type = "error", duration = 8)
      return()
    }
    
    user <- current_user()
    name <- session$userData$pending_entry_name
    if (is.null(user) || is.null(name)) {
      showNotification("Something went wrong — please try again.",
                       type = "error")
      return()
    }
    
    picks <- selected_players() |> select(player_id, position)
    
    result <- tryCatch({
      entry_id <- create_entry(user$email, name, user$display_name)
      save_roster_picks(entry_id, picks)
      list(ok = TRUE, entry_id = entry_id)
    }, error = function(e) {
      message("!!! submit failed: ", conditionMessage(e))
      list(ok = FALSE, error = conditionMessage(e))
    })
    
    if (result$ok) {
      selected_ids(integer(0))
      updateTextInput(session, iid("entry_name"), value = "")
      session$userData$pending_entry_name <- NULL
      session$sendCustomMessage("wc_clear_ls", list(key = "wc_lineup_state"))
      view_state("lineups")
      
      showModal(modalDialog(
        title = "Lineup submitted",
        tagList(
          p(paste0("Your lineup \"", name, "\" has been submitted.")),
          p("Pay your entry fee on Revolut to complete entry into the contest. ",
            "Entry monies are held by Revolut (an independent third party) — ",
            "funds are not held by the organiser, and the contest takes no rake.")
        ),
        footer = tagList(
          modalButton("Done"),
          payment_link(label = "Pay via Revolut →", class = "btn btn-primary")
        ),
        easyClose = TRUE
      ))
    } else {
      showNotification(paste0("Submit failed: ", result$error),
                       type = "error", duration = 10)
    }
  })
  
  observeEvent(input[[iid("confirm_save_lineup")]], {
    message(">>> OBSERVE ", iid("confirm_save_lineup"))
    removeModal()
    
    if (is_locked()) {
      message("    !! save blocked by fresh is_locked()")
      showNotification("Contest is locked. No further submissions or changes.",
                       type = "error", duration = 8)
      return()
    }
    
    entry_id <- editing_entry_id()
    if (is.null(entry_id)) {
      showNotification("Something went wrong — no entry selected.",
                       type = "error")
      return()
    }
    
    picks <- selected_players() |> select(player_id, position)
    
    result <- tryCatch({
      update_entry_picks(entry_id, picks)
      list(ok = TRUE)
    }, error = function(e) {
      message("!!! save failed: ", conditionMessage(e))
      list(ok = FALSE, error = conditionMessage(e))
    })
    
    if (result$ok) {
      showNotification("Changes saved.", type = "message", duration = 4)
      baseline_ids(selected_ids())
      on_save_success()
    } else {
      showNotification(paste0("Save failed: ", result$error),
                       type = "error", duration = 10)
    }
  })
}

# --- Standalone creator server wrapper --------------------------------

lineup_creator_server_logic <- function(input, output, session,
                                        current_user, view_state,
                                        is_locked_reactive) {
  lineup_picker_server_logic(
    input, output, session,
    mode               = "new",
    current_user       = current_user,
    view_state         = view_state,
    is_locked_reactive = is_locked_reactive
  )
}