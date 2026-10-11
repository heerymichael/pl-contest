# My Lineups: list of the user's existing lineups (oldest first), with
# an embedded editor that appears below when a lineup is clicked.
# Editor uses the shared lineup_picker in mode = "edit".
#
# Transfer window: post-lock the editor stays read-only EXCEPT while the
# Gameweek 3 window is open, when it becomes the transfer editor. That is
# driven by can_edit_reactive (from app.R: !locked || window open) and the
# window-open baseline roster (transfer_baseline() in R/sheets.R), both
# passed into the picker.

message(">>> SOURCED R/my_lineups.R at ", format(Sys.time()))

library(shiny)
library(dplyr)

# --- VIEW ---------------------------------------------------------------

my_lineups_view <- function() {
  div(
    class = "view",
    div(
      class = "lineup-header",
      h2("My Lineups")
    ),
    div(
      class = "payment-banner",
      span(class = "payment-banner-text",
           "Entries are paid separately via Revolut."),
      payment_link(label = "Pay via Revolut →",
                   class = "btn btn-primary btn-sm payment-banner-btn")
    ),
    uiOutput("my_lineups_list"),
    div(
      class = "lineup-editor-area",
      uiOutput("my_lineups_editor")
    )
  )
}

# --- SERVER LOGIC -------------------------------------------------------

my_lineups_server_logic <- function(input, output, session,
                                    current_user, view_state,
                                    is_locked_reactive,
                                    can_edit_reactive = NULL,
                                    window_state_reactive = reactive("none")) {
  
  message(">>> ENTER my_lineups_server_logic")
  
  if (is.null(can_edit_reactive)) {
    can_edit_reactive <- reactive(!isTRUE(is_locked_reactive()))
  }
  
  selected_lineup_id <- reactiveVal(NULL)
  editor_dirty       <- reactiveVal(FALSE)
  pending_switch_to  <- reactiveVal(NULL)
  list_refresh       <- reactiveVal(0L)
  
  user_lineups <- reactive({
    list_refresh()
    user <- current_user()
    if (is.null(user)) return(NULL)
    rows <- get_entries_for_user(user$email)
    if (nrow(rows) == 0) return(rows)
    rows |> arrange(created_at)
  })
  
  # Deliberately a LIVE read, not get_picks_for_entry_local().
  # roster_picks is writable state and the contest snapshot is only
  # valid post-lock — serving the editor from it pre-lock would load a
  # stale roster and, worse, set the dirty-tracking baseline from stale
  # data, so a save could silently revert an earlier edit. The player
  # join inside get_picks_for_entry() is served locally; only the picks
  # themselves cost a round-trip.
  current_lineup_picks <- reactive({
    eid <- selected_lineup_id()
    if (is.null(eid)) return(integer(0))
    picks <- get_picks_for_entry(eid)
    as.integer(picks$player_id)
  })
  
  # The roster the entry held when the window opened — period 1 plus any
  # club-clash auto-subs — so the picker's transfer counter is cumulative
  # across saves. Only computed while the window is open (two extra live
  # reads); NULL otherwise, and the picker falls back to the loaded roster.
  # Re-evaluated on every lineup open, not cached: an entrant who saves
  # and reopens must see the same baseline, and it never changes within
  # the window.
  transfer_baseline_ids <- reactive({
    eid <- selected_lineup_id()
    if (is.null(eid)) return(NULL)
    if (!isTRUE(is_locked_reactive()) ||
        !identical(window_state_reactive(), "open")) return(NULL)
    message(">>> [my_lineups] computing transfer baseline for ", eid)
    cfg   <- get_config_cached()
    picks <- read_sheet(sheet_id(), sheet = "roster_picks", col_types = "c")
    trs   <- get_all_transfers()
    base  <- transfer_baseline(eid, picks, trs,
                               as.integer(cfg$transfer_effective_gw %||% 3L))
    message("    baseline: ", length(base), " players")
    as.integer(base)
  })
  
  # List UI ---------------------------------------------------------
  
  output$my_lineups_list <- renderUI({
    user <- current_user()
    if (is.null(user)) {
      return(div(class = "empty-state",
                 p("Log in to see your lineups.")))
    }
    
    rows <- user_lineups()
    if (is.null(rows) || nrow(rows) == 0) {
      return(div(
        class = "empty-state",
        p("You haven't built any lineups yet."),
        actionButton("my_lineups_build_first", "Build your first lineup",
                     class = "btn btn-primary")
      ))
    }
    
    sel_id <- selected_lineup_id()
    
    row_uis <- lapply(seq_len(nrow(rows)), function(i) {
      r <- rows[i, ]
      is_selected <- !is.null(sel_id) && sel_id == r$entry_id
      row_class <- paste("lineup-list-row",
                         if (is_selected) "lineup-list-row-selected")
      tags$div(
        class   = row_class,
        onclick = sprintf("Shiny.setInputValue('select_lineup', '%s', {priority: 'event'});",
                          r$entry_id),
        tags$span(class = "lineup-list-name", r$entry_name),
        tags$span(class = "lineup-list-date",
                  format(as.POSIXct(r$created_at), "%d %b %Y, %H:%M"))
      )
    })
    
    do.call(tagList, c(
      list(div(class = "lineup-list", row_uis))
    ))
  })
  
  # Editor UI -------------------------------------------------------
  
  output$my_lineups_editor <- renderUI({
    eid <- selected_lineup_id()
    if (is.null(eid)) return(NULL)
    locked   <- isTRUE(is_locked_reactive())
    can_edit <- isTRUE(can_edit_reactive())
    
    notice <- if (locked && can_edit) {
      cfg <- get_config_cached()
      div(class = "locked-notice locked-notice--window",
          sprintf(paste("Transfer window open \u2014 you can change up to %d",
                        "players. Changes apply from Gameweek %d."),
                  as.integer(cfg$transfers_max %||% 2L),
                  as.integer(cfg$transfer_effective_gw %||% 3L)))
    } else if (locked) {
      div(class = "locked-notice",
          "This lineup is locked \u2014 you can view but not edit.")
    } else NULL
    
    tagList(notice, lineup_editor_ui())
  })
  
  lineup_picker_server_logic(
    input, output, session,
    mode               = "edit",
    current_user       = current_user,
    view_state         = view_state,
    initial_picks      = current_lineup_picks,
    editing_entry_id   = selected_lineup_id,
    is_locked_reactive = is_locked_reactive,
    can_edit_reactive  = can_edit_reactive,
    transfer_baseline_ids = transfer_baseline_ids,
    on_dirty_change    = function(is_dirty) { editor_dirty(is_dirty) },
    on_save_success    = function() {
      message(">>> [my_lineups] save success — closing editor")
      selected_lineup_id(NULL)
      list_refresh(isolate(list_refresh()) + 1L)
    }
  )
  
  # Row click — gated by dirty-confirm if needed -------------------
  
  observeEvent(input$select_lineup, {
    target <- input$select_lineup
    if (is.null(target) || !nzchar(target)) return()
    
    current <- selected_lineup_id()
    if (!is.null(current) && current == target) return()
    
    if (editor_dirty()) {
      pending_switch_to(target)
      showModal(modalDialog(
        title = "Discard unsaved changes?",
        "You have unsaved changes to the current lineup. Switching will discard them.",
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_discard_changes", "Discard changes",
                       class = "btn btn-danger")
        ),
        easyClose = TRUE
      ))
      return()
    }
    
    selected_lineup_id(target)
  })
  
  observeEvent(input$confirm_discard_changes, {
    removeModal()
    target <- pending_switch_to()
    pending_switch_to(NULL)
    if (!is.null(target)) {
      editor_dirty(FALSE)
      selected_lineup_id(target)
    }
  })
  
  observeEvent(input$my_lineups_build_first, {
    view_state("lineup")
  })
  
  observeEvent(view_state(), {
    if (view_state() != "lineups") {
      selected_lineup_id(NULL)
      editor_dirty(FALSE)
    }
  })
  
  observeEvent(current_user(), {
    if (is.null(current_user())) {
      selected_lineup_id(NULL)
      editor_dirty(FALSE)
    }
  }, ignoreInit = TRUE)
}