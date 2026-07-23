# Modal-based signup and login.
# Replaces the previous full-page landing/register/login flow.
# Triggered by header Login/Signup buttons, or by the lineup-creator
# Submit button when the user isn't yet authenticated.
# Also hosts the change-password modal (header button, logged-in only).

message(">>> SOURCED R/auth_modals.R at ", format(Sys.time()))

library(shiny)

# --- MODAL UI ------------------------------------------------------------

show_signup_modal <- function() {
  showModal(modalDialog(
    title = "Sign up",
    tagList(
      p("Pick a username (3–30 characters, letters/numbers/underscores/hyphens only)."),
      textInput("mod_signup_username", "Username"),
      textInput("mod_signup_display_name", "Display name"),
      helpText("Your display name will be shown on the leaderboard when the
                contest is live. It cannot be changed later."),
      textInput("mod_signup_email", "Email"),
      passwordInput("mod_signup_password", "Password (min 8 characters)"),
      passwordInput("mod_signup_password_confirm", "Confirm password"),
      uiOutput("mod_signup_message")
    ),
    footer = tagList(
      actionButton("mod_signup_switch_to_login", "Log in instead",
                   class = "btn btn-link"),
      modalButton("Cancel"),
      actionButton("mod_signup_submit", "Create account",
                   class = "btn btn-primary")
    ),
    easyClose = FALSE,
    size = "m"
  ))
}

show_login_modal <- function() {
  showModal(modalDialog(
    title = "Log in",
    tagList(
      textInput("mod_login_username", "Username"),
      passwordInput("mod_login_password", "Password"),
      uiOutput("mod_login_message")
    ),
    footer = tagList(
      actionButton("mod_login_switch_to_signup", "Sign up instead",
                   class = "btn btn-link"),
      modalButton("Cancel"),
      actionButton("mod_login_submit", "Log in",
                   class = "btn btn-primary")
    ),
    easyClose = FALSE,
    size = "m"
  ))
}

show_change_password_modal <- function() {
  showModal(modalDialog(
    title = "Change password",
    tagList(
      passwordInput("mod_chpw_current", "Current password"),
      passwordInput("mod_chpw_new", "New password (min 8 characters)"),
      passwordInput("mod_chpw_confirm", "Confirm new password"),
      uiOutput("mod_chpw_message")
    ),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("mod_chpw_submit", "Change password",
                   class = "btn btn-primary")
    ),
    easyClose = FALSE,
    size = "m"
  ))
}

# --- SERVER LOGIC --------------------------------------------------------

auth_modals_server_logic <- function(input, output, session, current_user) {
  
  message(">>> ENTER auth_modals_server_logic")
  
  # Header buttons --------------------------------------------------
  
  observeEvent(input$header_signup, {
    message(">>> OBSERVE header_signup")
    show_signup_modal()
  })
  
  observeEvent(input$header_login, {
    message(">>> OBSERVE header_login")
    show_login_modal()
  })
  
  observeEvent(input$header_change_password, {
    message(">>> OBSERVE header_change_password")
    if (is.null(current_user())) return()
    show_change_password_modal()
  })
  
  # Switch links between modals --------------------------------------
  
  observeEvent(input$mod_signup_switch_to_login, {
    message(">>> OBSERVE mod_signup_switch_to_login")
    removeModal()
    show_login_modal()
  })
  
  observeEvent(input$mod_login_switch_to_signup, {
    message(">>> OBSERVE mod_login_switch_to_signup")
    removeModal()
    show_signup_modal()
  })
  
  # Signup submit ---------------------------------------------------
  
  observeEvent(input$mod_signup_submit, {
    message(">>> OBSERVE mod_signup_submit")
    
    if (input$mod_signup_password != input$mod_signup_password_confirm) {
      output$mod_signup_message <- renderUI(
        div("Passwords do not match.", class = "register-error")
      )
      return()
    }
    
    result <- register_user(
      username     = input$mod_signup_username,
      email        = input$mod_signup_email,
      display_name = input$mod_signup_display_name,
      password     = input$mod_signup_password
    )
    
    if (is.null(result)) {
      message("    signup succeeded — auto-logging in")
      user <- verify_login(input$mod_signup_username, input$mod_signup_password)
      current_user(user)
      removeModal()
    } else {
      message("    signup failed: ", result)
      output$mod_signup_message <- renderUI(
        div(result, class = "register-error")
      )
    }
  })
  
  # Login submit ----------------------------------------------------
  
  observeEvent(input$mod_login_submit, {
    message(">>> OBSERVE mod_login_submit")
    
    user <- verify_login(input$mod_login_username, input$mod_login_password)
    
    if (is.null(user)) {
      message("    login failed")
      output$mod_login_message <- renderUI(
        div("Invalid username or password.", class = "register-error")
      )
    } else {
      message("    login succeeded for ", user$username)
      current_user(user)
      removeModal()
    }
  })
  
  # Change-password submit -------------------------------------------
  
  observeEvent(input$mod_chpw_submit, {
    message(">>> OBSERVE mod_chpw_submit")
    
    user <- current_user()
    if (is.null(user)) {
      removeModal()
      return()
    }
    
    # 1. Verify the current password.
    check <- verify_login(user$username, input$mod_chpw_current)
    if (is.null(check)) {
      message("    chpw failed: current password incorrect")
      output$mod_chpw_message <- renderUI(
        div("Current password is incorrect.", class = "register-error")
      )
      return()
    }
    
    # 2. New and confirm must match.
    if (input$mod_chpw_new != input$mod_chpw_confirm) {
      output$mod_chpw_message <- renderUI(
        div("New passwords do not match.", class = "register-error")
      )
      return()
    }
    
    # 3. Write the new hash (validates min length internally).
    result <- update_user_password(user$username, input$mod_chpw_new)
    
    if (is.null(result)) {
      message("    chpw succeeded for ", user$username)
      removeModal()
      showNotification("Password changed.", type = "message", duration = 4)
    } else {
      message("    chpw failed: ", result)
      output$mod_chpw_message <- renderUI(
        div(result, class = "register-error")
      )
    }
  })
}