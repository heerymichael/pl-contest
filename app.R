# GAFFER · Fantasy XV — Premier League 26/27 contest — main app
library(shiny)
library(dplyr)


source("R/sheets.R")
source("R/cache.R")
source("R/gameweeks.R")
source("R/lock.R")
source("R/transfers.R")
source("R/users.R")
source("R/badges.R")
source("R/pitch.R")
source("R/nav.R")
source("R/lineup_creator.R")
source("R/my_lineups.R")
source("R/scoring.R")
source("R/bestball.R")
source("R/leaderboard.R")
source("R/scores_2526.R")
source("R/auth_modals.R")
source("R/utils.R")

gs_auth()

# Warm the contest_config cache at startup so the first session's first
# flush doesn't pay a Sheets round-trip just to decide the lock state.
message(">>> warming contest_config cache at startup")
invisible(get_config_cached())

message(">>> SOURCED app.R at ", format(Sys.time()))

# Inline JS: localStorage bridge + countdown ticker.
client_js <- HTML("
  // --- localStorage bridge -----------------------------------------
  Shiny.addCustomMessageHandler('wc_save_ls', function(data) {
    try { localStorage.setItem(data.key, JSON.stringify(data.value)); } catch(e) {}
  });
  Shiny.addCustomMessageHandler('wc_clear_ls', function(data) {
    try { localStorage.removeItem(data.key); } catch(e) {}
  });
  Shiny.addCustomMessageHandler('wc_load_ls', function(data) {
    try {
      var v = localStorage.getItem(data.key);
      Shiny.setInputValue(data.input_id, v ? JSON.parse(v) : null, {priority: 'event'});
    } catch(e) {
      Shiny.setInputValue(data.input_id, null, {priority: 'event'});
    }
  });

  // --- Picker mobile view toggle -----------------------------------
  // Swaps picker-mobile-lineup / picker-mobile-pool on the picker root.
  // CSS (≤768px) drives which panel is visible; desktop ignores it.
  Shiny.addCustomMessageHandler('wc_set_picker_mobile_view', function(data) {
    var el = document.getElementById(data.id);
    if (!el) return;
    el.classList.remove('picker-mobile-lineup', 'picker-mobile-pool');
    el.classList.add('picker-mobile-' + data.view);
  });

  // --- Countdown ----------------------------------------------------
  // The banner element carries its own target as data-until (ISO UTC)
  // and, optionally, a fixed band class as data-band. Reading them off
  // the element every tick means a re-rendered banner with a new target
  // (lock -> window open -> window close) just works. Zero fires
  // wc_countdown_fired once per target; R bumps lock_tick and re-renders.
  (function() {
    var firedFor = null;

    function pad(n) { return n.toString().padStart(2, '0'); }

    function fmtSecs(secs) {
      var d = Math.floor(secs / 86400);
      var h = Math.floor((secs % 86400) / 3600);
      var m = Math.floor((secs % 3600) / 60);
      var s = Math.floor(secs % 60);
      if (d > 0) return d + 'd ' + pad(h) + 'h ' + pad(m) + 'm ' + pad(s) + 's';
      if (h > 0) return pad(h) + 'h ' + pad(m) + 'm ' + pad(s) + 's';
      return pad(m) + 'm ' + pad(s) + 's';
    }

    function bandClass(secs) {
      if (secs <= 0) return 'banner-locked';
      if (secs < 3600) return 'banner-urgent';
      if (secs < 86400) return 'banner-warn';
      return 'banner-neutral';
    }

    function tick() {
      var banner  = document.getElementById('countdown-banner');
      var timer   = document.getElementById('countdown-banner-timer');
      if (!banner || !timer) return;
      var until = banner.getAttribute('data-until');
      if (!until) return;
      var untilMs = new Date(until).getTime();
      var message = banner.querySelector('.countdown-banner-message');
      var fixed   = banner.getAttribute('data-band');

      var secs = Math.floor((untilMs - Date.now()) / 1000);
      banner.className = 'countdown-banner ' + (fixed || bandClass(secs));

      if (secs <= 0) {
        var zero = banner.getAttribute('data-zero');
        if (message && zero) message.textContent = zero;
        timer.textContent = '';
        timer.style.display = 'none';
        if (firedFor !== until) {
          firedFor = until;
          console.log('[wc] countdown reached zero for', until, '- firing wc_countdown_fired');
          Shiny.setInputValue('wc_countdown_fired', until, {priority: 'event'});
        }
        return;
      }

      timer.style.display = '';
      timer.textContent = fmtSecs(secs);
    }

    setInterval(tick, 1000);
  })();

  // --- Mobile nav toggle -------------------------------------------
  document.addEventListener('click', function(e) {
    var btn = e.target.closest('#nav-hamburger');
    if (btn) {
      var bar = document.querySelector('.nav-bar');
      if (bar) bar.classList.toggle('nav-bar-open');
      return;
    }
    // Tapping a nav link inside the open menu closes it.
    var link = e.target.closest('.nav-bar a, .nav-bar button');
    if (link && link.id !== 'nav-hamburger') {
      var bar2 = document.querySelector('.nav-bar');
      if (bar2) bar2.classList.remove('nav-bar-open');
    }
  });
")

# Top-level UI
ui <- fluidPage(
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    # Cache-buster: the file's mtime as a query param means browsers
    # fetch fresh CSS whenever styles.css changes (locally or on
    # redeploy) instead of serving a stale cached copy.
    tags$link(rel = "stylesheet", type = "text/css",
              href = paste0("styles.css?v=",
                            as.integer(file.mtime("www/styles.css")))),
    tags$link(rel = "stylesheet", type = "text/css",
              href = "https://fonts.googleapis.com/css2?family=Archivo:wght@400;500;600;700&family=Saira+Condensed:wght@500;600;700;800&display=swap"),
    tags$script(client_js)
  ),
  uiOutput("page")
)

# App shell: countdown banner (top of page) + header + current view.
# When locked, the "lineup" view is normalised to "leaderboard" *before*
# the switch — so the leaderboard is the landing page post-lock (for
# logged-in and logged-out users alike), anyone mid-session when the
# countdown fires flips straight to it, and the nav highlights the
# Leaderboard link correctly. locked_view() in R/lock.R is no longer
# reachable from here; left in place deliberately.
app_shell_ui <- function(current_username, current_view, locked,
                         window_state = "none") {
  if (locked && current_view == "lineup") current_view <- "leaderboard"
  
  view <- switch(current_view,
                 "lineup"      = lineup_creator_view(),
                 "lineups"     = my_lineups_view(),
                 "leaderboard" = leaderboard_view(),
                 "rules"       = rules_view(),
                 "scores"      = scores_2526_view(),
                 if (locked) leaderboard_view() else lineup_creator_view()
  )
  
  tagList(
    countdown_banner_ui(locked, window_state),
    nav_header(current_username, current_view, locked),
    view
  )
}

# Server ---------------------------------------------------------------------

server <- function(input, output, session) {
  current_user <- reactiveVal(NULL)
  
  # Post-lock the lineup view is unreachable, so initialise the view
  # state correctly at session start. app_shell_ui() normalises its own
  # copy for rendering, but view-gated server reactives (the
  # leaderboard's lb_data) read this reactiveVal directly — if it says
  # "lineup" while the UI shows the leaderboard, the board renders
  # blank. Initialising here (rather than flipping in an observer)
  # means one clean page render with no mid-flush view change.
  view_state <- reactiveVal(if (is_locked_cached()) "leaderboard" else "lineup")
  message(">>> view_state initialised to: ", isolate(view_state()))
  
  lock_tick <- reactiveVal(0L)
  
  is_locked_reactive <- reactive({
    lock_tick()
    is_locked_cached()
  })
  
  # Transfer-window state and the "may this entry be edited" answer.
  # Both hang off lock_tick so the banner reaching zero (lock, window
  # open, window close) re-evaluates them in the same flush.
  window_state_reactive <- reactive({
    lock_tick()
    transfer_window_state_cached()
  })
  
  can_edit_reactive <- reactive({
    !is_locked_reactive() || identical(window_state_reactive(), "open")
  })
  
  output$page <- renderUI({
    user <- current_user()
    username <- if (!is.null(user)) user$display_name else NULL
    locked <- is_locked_reactive()
    app_shell_ui(username, view_state(), locked, window_state_reactive())
  })
  
  # The banner's countdown hit zero — lock, window open, or window
  # close. One bump re-renders everything that depends on time.
  observeEvent(input$wc_countdown_fired, {
    message(">>> OBSERVE wc_countdown_fired (", input$wc_countdown_fired,
            ") — bumping lock_tick")
    lock_tick(isolate(lock_tick()) + 1L)
    # Anyone sitting on the lineup view when the contest locks flips to
    # the leaderboard — keeps the server-side view state in step with
    # app_shell_ui()'s post-lock normalisation.
    if (isolate(view_state()) == "lineup" && is_locked_cached()) {
      message(">>> wc_countdown_fired — flipping view_state to leaderboard")
      view_state("leaderboard")
    }
  })
  
  observeEvent(input$logout, {
    message(">>> OBSERVE logout")
    current_user(NULL)
    # Same lock-aware default as session start — resetting to "lineup"
    # post-lock would recreate the blank-leaderboard landing state.
    view_state(if (is_locked_cached()) "leaderboard" else "lineup")
    session$sendCustomMessage("wc_clear_ls", list(key = "wc_lineup_state"))
  })
  
  observeEvent(input$nav_lineup,      { view_state("lineup") })
  observeEvent(input$nav_lineups,     { view_state("lineups") })
  observeEvent(input$nav_leaderboard, { view_state("leaderboard") })
  observeEvent(input$nav_rules,       { view_state("rules") })
  observeEvent(input$nav_scores,      { view_state("scores") })
  
  auth_modals_server_logic(input, output, session, current_user)
  lineup_creator_server_logic(input, output, session, current_user,
                              view_state, is_locked_reactive)
  my_lineups_server_logic(input, output, session, current_user,
                          view_state, is_locked_reactive,
                          can_edit_reactive     = can_edit_reactive,
                          window_state_reactive = window_state_reactive)
  leaderboard_server_logic(input, output, session, current_user,
                           view_state, is_locked_reactive)
  scores_2526_server_logic(input, output, session)
}

shinyApp(ui, server)