# 2025-26 season scores exhibit (pl-contest). v2
#
# Static reference view: every 25/26 PL player's season, scored with
# the GAFFER ruleset (data/pl2526_scores.rds from build_2526_scores.R,
# which runs the live R/scoring.R engine with a per-category breakdown
# asserted against it).
#
# v2: events/points display toggle across ALL scoring categories;
# badge-only club column; heavy-surname player names; 1dp point
# formatting; position filter defaults to all outfielders (GK stats
# dominate several columns, so keepers get their own view); club
# filter choices baked into the view (dynamic updates don't reach
# inputs created after server start).

message(">>> SOURCED R/scores_2526.R at ", format(Sys.time()))

library(shiny)
library(dplyr)

.scores_2526 <- if (file.exists("data/pl2526_scores.rds")) {
  out <- readRDS("data/pl2526_scores.rds")
  # Exhibit floor: exclude small samples (fewer than 4 apps or fewer
  # than 360 minutes) — their totals and PP90 are noise. View-only:
  # the full table still feeds the pool's pts_2526/pp90_2526 join in
  # snapshot_pl_reference.R.
  n_all <- nrow(out)
  out <- out |> filter(apps >= 4, minutes >= 360)
  .sc26_excluded <- n_all - nrow(out)
  message("    [scores2526] loaded ", n_all, " players; showing ",
          nrow(out), " (excluded ", .sc26_excluded,
          " with <4 apps or <360 mins)")
  out
} else {
  message("    [scores2526] data/pl2526_scores.rds MISSING — view will ",
          "show a placeholder. Run build_2526_scores.R.")
  .sc26_excluded <- NULL
  NULL
}

# Category registry: display order; count column, points column, labels.
.sc_cats <- list(
  list(id = "g",    label = "G",    n = "g_n",       p = "g_p"),
  list(id = "a",    label = "A",    n = "a_n",       p = "a_p"),
  list(id = "sh",   label = "Sh",   n = "sh_n",      p = "sh_p"),
  list(id = "sot",  label = "SoT",  n = "sot_n",     p = "sot_p"),
  list(id = "tkl",  label = "Tkl",  n = "tkl_n",     p = "tkl_p"),
  list(id = "int",  label = "Int",  n = "int_n",     p = "int_p"),
  list(id = "yc",   label = "YC",   n = "yc_n",      p = "yc_p"),
  list(id = "rc",   label = "RC",   n = "rc_n",      p = "rc_p"),
  list(id = "og",   label = "OG",   n = "og_n",      p = "og_p"),
  list(id = "cs",   label = "CS",   n = "cs_n",      p = "cs_p"),
  list(id = "res",  label = "Res",  n = "wd",        p = "res_p"),
  list(id = "sv",   label = "Saves", n = "sv_n",     p = "sv_p"),
  list(id = "ps",   label = "PenSv", n = "ps_n",     p = "ps_p"),
  list(id = "conc", label = "Conc", n = "conc_gk_n", p = "conc_p")
)

.sc_fixed <- list(
  list(id = "player",  label = "Player"),
  list(id = "club",    label = "Club"),
  list(id = "position", label = "Pos"),
  list(id = "apps",    label = "Apps"),
  list(id = "minutes", label = "Mins")
)

scores_2526_view <- function() {
  club_select <- if (is.null(.scores_2526)) {
    selectInput("sc26_filter_club", "Club", choices = c("All clubs" = "ALL"))
  } else {
    cl <- .scores_2526 |> distinct(club, club_code) |> arrange(club)
    labels <- vapply(seq_len(nrow(cl)), function(i) {
      code <- tolower(cl$club_code[i])
      if (file.exists(file.path("www", "logos", paste0(code, ".svg")))) {
        sprintf('<img class="badge-icon badge-icon-md" src="logos/%s.svg" alt=""/>%s',
                code, cl$club[i])
      } else {
        sprintf('<span class="scores-badge-fallback">%s</span> %s',
                cl$club_code[i], cl$club[i])
      }
    }, character(1))
    selectizeInput(
      "sc26_filter_club", "Club",
      choices  = setNames(c("ALL", cl$club), c("All clubs", labels)),
      selected = "ALL",
      options  = list(render = I("{
        option: function(item, escape) { return '<div class=\"badge-option\">' + item.label + '</div>'; },
        item:   function(item, escape) { return '<div class=\"badge-option\">' + item.label + '</div>'; }
      }"))
    )
  }
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
         behaves before you draft. Toggle between the number of scoring
         events and the points each category earned. Click any column
         header to sort."
      ),
      fluidRow(
        column(3, selectInput("sc26_filter_position", "Position",
                              choices = c("All positions (excl GK)" = "OUTFIELD",
                                          "GK"  = "GK",
                                          "DEF" = "DEF",
                                          "MID" = "MID",
                                          "FWD" = "FWD"))),
        column(3, club_select),
        column(3, textInput("sc26_filter_name", "Name search",
                            placeholder = "type to search\u2026")),
        column(3, radioButtons("sc26_mode", "Show",
                               choices = c("Events" = "n", "Points" = "p"),
                               selected = "n", inline = TRUE))
      ),
      div(
        class = "scores-table-scroll",
        uiOutput("sc26_table_ui")
      ),
      p(
        class = "scores-footnote",
        "Only matches where the player was on the pitch are counted.
         Res shows wins-draws as events and result points (GK win +5 /
         draw +2, DEF win +2 / draw +1) in points view; Conc is goals
         conceded while a keeper played. OG is own goals (\u22125);
         PenSv is in-play penalty saves (+3), extracted from match
         shotmaps.",
        if (!is.null(.sc26_excluded))
          paste0(" The table lists players with at least 4 appearances",
                 " and 360 minutes played in 25/26 \u2014 ",
                 .sc26_excluded, " smaller-sample players are excluded.")
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
  
  sc_sort <- reactiveVal(list(col = "total_points", desc = TRUE))
  
  observeEvent(input$sc26_sort_col, {
    cur <- sc_sort()
    col <- input$sc26_sort_col
    if (identical(cur$col, col)) {
      sc_sort(list(col = col, desc = !cur$desc))
    } else {
      sc_sort(list(col = col,
                   desc = !col %in% c("player_name", "club", "position")))
    }
  })
  
  output$sc26_table_ui <- renderUI({
    df   <- .scores_2526
    mode <- input$sc26_mode %||% "n"
    
    fp <- input$sc26_filter_position %||% "OUTFIELD"
    fc <- input$sc26_filter_club
    fn <- input$sc26_filter_name
    
    df <- if (fp == "OUTFIELD") df |> filter(position != "GK")
    else df |> filter(position == fp)
    if (!is.null(fc) && fc != "ALL") df <- df |> filter(club == fc)
    if (!is.null(fn) && nzchar(trimws(fn))) {
      df <- df |> filter(grepl(trimws(fn), player_name,
                               ignore.case = TRUE))
    }
    
    # sortable column id -> actual data column for the current mode
    col_for <- function(cid) {
      if (cid %in% c("player_name", "club", "position",
                     "apps", "minutes", "total_points", "pp90")) return(cid)
      cat <- Filter(function(x) x$id == cid, .sc_cats)[[1]]
      if (mode == "n") {
        if (cat$n == "wd") "w_n" else cat$n
      } else cat$p
    }
    
    srt <- sc_sort()
    scol <- col_for(srt$col)
    df <- df[order(df[[scol]], decreasing = srt$desc), ]
    
    if (nrow(df) == 0) {
      return(div(class = "empty-state",
                 p("No players match these filters.")))
    }
    
    th <- function(cid, label, num = TRUE, center = FALSE) {
      active <- identical(sc_sort()$col, cid)
      arrow  <- if (!active) "" else if (sc_sort()$desc) " \u25be" else " \u25b4"
      tags$th(
        class = paste("scores-th",
                      if (center) "scores-center" else if (num) "scores-num",
                      if (active) "scores-th-active"),
        onclick = sprintf(
          "Shiny.setInputValue('sc26_sort_col', '%s', {priority: 'event'});",
          cid),
        paste0(label, arrow)
      )
    }
    
    header <- tags$tr(
      th("player_name", "Player", num = FALSE),
      th("club",        "Club",   center = TRUE),
      th("position",    "Pos",    center = TRUE),
      th("apps",        "Apps",   center = TRUE),
      th("minutes",     "Mins"),
      lapply(.sc_cats, function(cat)
        th(cat$id, cat$label, center = identical(cat$id, "res"))),
      th("total_points", "Points"),
      th("pp90",         "PP90")
    )
    
    fmt_n <- function(x) format(round(x), big.mark = ",")
    fmt_p <- function(x) sprintf("%.1f", x)
    
    body <- lapply(seq_len(nrow(df)), function(i) {
      r <- df[i, ]
      badge <- tags$img(
        class = "scores-badge",
        src   = paste0("logos/", tolower(r$club_code), ".svg"),
        alt   = r$club_code, title = r$club,
        onerror = sprintf(
          "this.outerHTML='<span class=\"scores-badge-fallback\">%s</span>';",
          r$club_code)
      )
      cat_cells <- lapply(.sc_cats, function(cat) {
        val <- if (mode == "n") {
          if (cat$n == "wd") paste0(r$w_n, "-", r$d_n) else fmt_n(r[[cat$n]])
        } else fmt_p(r[[cat$p]])
        tags$td(class = if (identical(cat$id, "res")) "scores-center"
                else "scores-num", val)
      })
      tags$tr(
        class = "scores-tr",
        tags$td(tags$span(class = "scores-player",
                          if (nzchar(r$first_name))
                            tags$span(class = "scores-first", r$first_name),
                          tags$span(class = "scores-surname", r$surname))),
        tags$td(class = "scores-club-cell", badge),
        tags$td(class = "scores-center", r$position),
        tags$td(class = "scores-center", fmt_n(r$apps)),
        tags$td(class = "scores-num", fmt_n(r$minutes)),
        cat_cells,
        tags$td(class = "scores-num scores-pts", fmt_p(r$total_points)),
        tags$td(class = "scores-num", fmt_p(r$pp90))
      )
    })
    
    tags$table(class = "scores-table",
               tags$thead(header), tags$tbody(body))
  })
}