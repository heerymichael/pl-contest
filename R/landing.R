# Landing page and registration form UI

library(shiny)

# Landing page — two big buttons: NEW USER or EXISTING USER
landing_ui <- function() {
  fluidPage(
    tags$head(tags$style(HTML("
      .landing-container {
        max-width: 500px;
        margin: 100px auto;
        text-align: center;
      }
      .landing-title {
        font-size: 36px;
        font-weight: 700;
        margin-bottom: 40px;
      }
      .landing-btn {
        display: block;
        width: 100%;
        padding: 20px;
        margin: 15px 0;
        font-size: 18px;
        border-radius: 8px;
      }
    "))),
    div(
      class = "landing-container",
      h1("World Cup 2026 Challenge", class = "landing-title"),
      actionButton(
        "go_register", "NEW USER — Sign up",
        class = "btn btn-primary landing-btn"
      ),
      actionButton(
        "go_login", "EXISTING USER — Log in",
        class = "btn btn-default landing-btn"
      )
    )
  )
}

# Registration form UI
register_ui <- function() {
  fluidPage(
    tags$head(tags$style(HTML("
      .register-container {
        max-width: 500px;
        margin: 60px auto;
        padding: 30px;
        border: 1px solid #ddd;
        border-radius: 8px;
      }
      .register-error {
        color: #c0392b;
        margin-top: 10px;
        font-weight: 500;
      }
      .register-success {
        color: #27ae60;
        margin-top: 10px;
        font-weight: 500;
      }
    "))),
    div(
      class = "register-container",
      h2("Create your account"),
      p("Pick a username (3–30 characters, letters/numbers/underscores/hyphens only).
        Your username will be visible on the leaderboard and cannot be changed later."),
      textInput("reg_username", "Username", placeholder = "e.g. MikeB"),
      textInput("reg_email", "Email", placeholder = "you@example.com"),
      passwordInput("reg_password", "Password",
                    placeholder = "Minimum 8 characters"),
      passwordInput("reg_password_confirm", "Confirm password"),
      actionButton("submit_register", "Create account",
                   class = "btn btn-primary"),
      actionButton("back_to_landing", "Back",
                   class = "btn btn-default"),
      uiOutput("register_message")
    )
  )
}