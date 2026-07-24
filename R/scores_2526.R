# 2025-26 season scores exhibit (pl-contest).
#
# Static reference view: every 25/26 PL player's season stats and what
# they would have scored under the GAFFER ruleset. Data is the bundled
# data/pl2526_scores.rds, produced by build_2526_scores.R (which runs
# the season through the live R/scoring.R engine — re-run it after any
# scoring change and redeploy).
#
# No Sheets reads, no reactivity beyond filter/sort state. If the RDS
# is missing from the bundle the view degrades to an explanatory
# message rather than erroring.

message(">>> SOURCED R/scores_2526.R at ", format(Sys.time()))

library(shiny)
library(dplyr)

.scores_2526 <- if (file.exists("data/pl2526_scores.rds")) {
  out <- readRDS("data/pl2526_scores.rds")
  message("    [scores2526] loaded ", nrow(out), " players")
  out
} else {
  message("    [scores2526] data/pl2526_scores.rds MISSING — view will ",
          "show a placeholder. Run build_2526_scores.R.")
  NULL
}

# Column registry: id -> header label + numeric flag (drives sorting
# and right-alignment). Order here is display order.
.sc_cols <- list(
  player_name   = list(label = "Player",        num = FALSE),
  club          = list(label = "Club",          num = FALSE),
  position      = list(label = "Pos",           num = FALSE),
  apps          = list(label = "Apps",          num = TRUE),
  minutes       = list(label = "Mins",          num = TRUE),
  goals         = list(label = "G",             num = TRUE),
  assists       = list(label = "A",             num = TRUE),
  tackles       = list(label = "Tkl",           num = TRUE),
  interceptions = list(label = "Int",           num = TRUE),
  clean_sheets  = list(label = "CS",            num = TRUE),
  saves         = list(label = "Saves",         num = TRUE),
  total_points  = list(label = "Points",        num = TRUE),
  ppg           = list(label = "PPG",           num = TRUE)
)

scores_2526_view <- function() {
  div(
    class = "view",
    div(
      class = "lineup-header",
      div(
        div(class = "lineup-eyebrow", "Last season under GAFFER rules"),
        h2("2025-26 Scores")
      )
    ),
    div(
      class = "panel-card scores-panel",
      p(
        class = "scores-intro",
        "Every player's 2025-26 Premier League season, scored with this
         contest's ruleset — so you can see exactly how the scoring
         behaves before you draft. Tackles and interceptions make
         defenders live; result points reward keepers and defenders in
         winning sides. Click any column header to sort."
      ),
      fluidRow(
        column(4, selectInput("sc26_filter_position", "Position",
                              choices = c("All positions" = "ALL",
                                          "GK"  = "GK",
                                          "DEF" = "DEF",
                                          "MID" = "MID",
                                          "FWD" = "FWD"))),
        column(4, textInput("sc26_filter_name", "Name search",
                            placeholder = "type to search\u2026")),
        column(4, selectInput("sc26_filter_club", "Club",
                              choices = c("All clubs" = "ALL")))
      ),
      div(
        class = "scores-table-scroll",
        uiOutput("sc26_table_ui")
      ),
      p(
        class = "scores-footnote",
        "Season totals include every scoring action the data pipeline
         collects. Two rare events are not in the historic dataset and
         are excluded here: in-play penalty saves (+3) and own goals
         (\u22125). Both will score normally in the live contest."
      )
    )
  )
}

scores_2526_server_logic <- function(input, output, session) {

  message(">>> ENTER scores_2526_server_logic")

  if (is.null(.scores_2526)) {
    output$sc26_table_ui <- renderUI({
      div(class = "empty-state",
          p("The 2025-26 scores dataset isn't bundled in this deploy."))
    })
    return(invisible(NULL))
  }

  # Populate the club filter from the data (25/26 clubs, incl. the
  # three since relegated)
  updateSelectInput(session, "sc26_filter_club",
                    choices = c("All clubs" = "ALL",
                                sort(unique(.scores_2526$club))))

  # Sort state: column + direction. Default: points, descending.
  sc_sort <- reactiveVal(list(col = "total_points", desc = TRUE))

  observeEvent(input$sc26_sort_col, {
    cur <- sc_sort()
    col <- input$sc26_sort_col
    if (!col %in% names(.sc_cols)) return()
    if (identical(cur$col, col)) {
      sc_sort(list(col = col, desc = !cur$desc))
    } else {
      # New column: numeric columns start descending, text ascending
      sc_sort(list(col = col, desc = isTRUE(.sc_cols[[col]]$num)))
    }
  })

  output$sc26_table_ui <- renderUI({
    df <- .scores_2526

    fp <- input$sc26_filter_position
    fc <- input$sc26_filter_club
    fn <- input$sc26_filter_name

    if (!is.null(fp) && fp != "ALL") df <- df |> filter(position == fp)
    if (!is.null(fc) && fc != "ALL") df <- df |> filter(club == fc)
    if (!is.null(fn) && nzchar(trimws(fn))) {
      df <- df |> filter(grepl(trimws(fn), player_name,
                               ignore.case = TRUE))
    }

    srt <- sc_sort()
    ord <- order(df[[srt$col]], decreasing = srt$desc)
    df  <- df[ord, ]

    if (nrow(df) == 0) {
      return(div(class = "empty-state",
                 p("No players match these filters.")))
    }

    header_cells <- lapply(names(.sc_cols), function(cid) {
      meta   <- .sc_cols[[cid]]
      active <- identical(srt$col, cid)
      arrow  <- if (!active) "" else if (srt$desc) " \u25be" else " \u25b4"
      tags$th(
        class   = paste("scores-th",
                        if (meta$num) "scores-num",
                        if (active) "scores-th-active"),
        onclick = sprintf(
          "Shiny.setInputValue('sc26_sort_col', '%s', {priority: 'event'});",
          cid),
        paste0(meta$label, arrow)
      )
    })

    body_rows <- lapply(seq_len(nrow(df)), function(i) {
      r <- df[i, ]
      cells <- lapply(names(.sc_cols), function(cid) {
        val <- r[[cid]]
        tags$td(
          class = paste(if (isTRUE(.sc_cols[[cid]]$num)) "scores-num",
                        if (cid == "total_points") "scores-pts"),
          if (cid == "player_name") {
            tags$span(class = "scores-player", as.character(val))
          } else if (is.numeric(val)) {
            format(val, big.mark = ",")
          } else {
            as.character(val)
          }
        )
      })
      tags$tr(class = "scores-tr", cells)
    })

    tags$table(
      class = "scores-table",
      tags$thead(tags$tr(header_cells)),
      tags$tbody(body_rows)
    )
  })
}
