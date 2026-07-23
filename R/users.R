# User registration, login, and lookup helpers.
#
# Sheets-backed: user accounts live in the `users` tab of the contest
# Google Sheet (wc-contest-data). Replaces the previous SQLite store,
# which did not survive shinyapps.io instance restarts (ephemeral
# filesystem) — accounts written to the bundled DB were silently lost.
#
# Passwords are sodium hashes stored as text; sodium::password_store()
# output is a plain string and round-trips through a Sheet cell intact.
#
# All reads are fresh (no caching) — same reasoning as is_locked():
# auth decisions must never run against stale data.
#
# Function signatures are unchanged from the SQLite version, so
# auth_modals.R, app.R, and lineup_creator.R need no edits.

library(dplyr)
library(tibble)
library(sodium)

message(">>> SOURCED R/users.R at ", format(Sys.time()))

USERS_TAB <- "users"

# Explicit column types so every field — especially password_hash —
# always comes back as character rather than being type-guessed.
# Column order: username, username_lc, display_name, display_name_lc,
# email, email_lc, password_hash, is_admin (integer), created_at.
.users_col_types <- "cccccccic"

# Fresh read of the users tab. Internal helper — every auth function
# below goes through this.
read_users_tab <- function() {
  out <- read_sheet(sheet_id(), sheet = USERS_TAB,
                    col_types = .users_col_types)
  message("    [users] read users tab — ", nrow(out), " rows")
  out
}

# Initialise the users tab (run once; safe to re-run). Creates the tab
# with headers only if it doesn't already exist — never overwrites.
init_users_db <- function() {
  message(">>> init_users_db (Sheets)")
  existing_tabs <- googlesheets4::sheet_names(sheet_id())
  if (USERS_TAB %in% existing_tabs) {
    message("    [users] `", USERS_TAB, "` tab already exists — nothing to do")
    return(invisible(NULL))
  }
  header <- tibble(
    username        = character(0),
    username_lc     = character(0),
    display_name    = character(0),
    display_name_lc = character(0),
    email           = character(0),
    email_lc        = character(0),
    password_hash   = character(0),
    is_admin        = integer(0),
    created_at      = character(0)
  )
  googlesheets4::sheet_write(header, ss = sheet_id(), sheet = USERS_TAB)
  message("    [users] created `", USERS_TAB, "` tab with headers")
  invisible(NULL)
}

# Read all users (admin/diagnostic use).
read_all_users <- function() {
  read_users_tab()
}

# Username validation
validate_username <- function(username) {
  if (is.null(username) || username == "") {
    return("Username is required.")
  }
  if (nchar(username) < 3 || nchar(username) > 30) {
    return("Username must be between 3 and 30 characters.")
  }
  if (!grepl("^[A-Za-z0-9_-]+$", username)) {
    return("Username can only contain letters, numbers, underscores, and hyphens (no spaces).")
  }
  if (tolower(username) == "admin") {
    return("That username is reserved. Please choose another.")
  }
  NULL
}

# Display name validation. Shown on the leaderboard; 2-50 characters
# after trimming, spaces allowed, no character restrictions.
validate_display_name <- function(display_name) {
  if (is.null(display_name) || trimws(display_name) == "") {
    return("Display name is required.")
  }
  n <- nchar(trimws(display_name))
  if (n < 2 || n > 50) {
    return("Display name must be between 2 and 50 characters.")
  }
  NULL
}

# Email validation
validate_email <- function(email) {
  if (is.null(email) || email == "") {
    return("Email is required.")
  }
  if (!grepl("^[^@[:space:]]+@[^@[:space:]]+\\.[^@[:space:]]+$", email)) {
    return("Please enter a valid email address.")
  }
  NULL
}

# Password validation
validate_password <- function(password) {
  if (is.null(password) || password == "") {
    return("Password is required.")
  }
  if (nchar(password) < 8) {
    return("Password must be at least 8 characters.")
  }
  NULL
}

# Uniqueness checks — each does a fresh read of the users tab.

username_exists <- function(username) {
  users <- read_users_tab()
  if (nrow(users) == 0) return(FALSE)
  tolower(username) %in% users$username_lc
}

# Case-insensitive — "Mike B" blocks a later "mike b".
display_name_exists <- function(display_name) {
  users <- read_users_tab()
  if (nrow(users) == 0) return(FALSE)
  tolower(trimws(display_name)) %in% users$display_name_lc
}

email_exists <- function(email) {
  users <- read_users_tab()
  if (nrow(users) == 0) return(FALSE)
  tolower(email) %in% users$email_lc
}

# Register a new user. Returns NULL on success, error message string
# on failure. Appends one row to the `users` tab.
register_user <- function(username, email, display_name, password, is_admin = FALSE) {
  err <- validate_username(username);          if (!is.null(err)) return(err)
  err <- validate_display_name(display_name);  if (!is.null(err)) return(err)
  err <- validate_email(email);                if (!is.null(err)) return(err)
  err <- validate_password(password);          if (!is.null(err)) return(err)
  
  if (username_exists(username)) {
    return("That username is already taken. Please choose another.")
  }
  if (display_name_exists(display_name)) {
    return("That display name is already taken. Please choose another.")
  }
  if (email_exists(email)) {
    return("An account with that email already exists.")
  }
  
  display_name <- trimws(display_name)
  
  row <- tibble(
    username        = username,
    username_lc     = tolower(username),
    display_name    = display_name,
    display_name_lc = tolower(display_name),
    email           = email,
    email_lc        = tolower(email),
    password_hash   = sodium::password_store(password),
    is_admin        = as.integer(is_admin),
    created_at      = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )
  
  result <- tryCatch({
    append_sheet_row(USERS_TAB, row)
    NULL
  }, error = function(e) {
    message("!!! [users] register append failed: ", conditionMessage(e))
    "Something went wrong creating your account. Please try again."
  })
  
  if (is.null(result)) {
    message("    [users] registered user ", username)
  }
  result
}

# Verify login credentials. Returns the user row (1-row tibble) on
# success, NULL on failure.
verify_login <- function(username, password) {
  if (is.null(username) || is.null(password) || username == "" || password == "") {
    return(NULL)
  }
  
  users <- read_users_tab()
  if (nrow(users) == 0) {
    message("    [users] verify_login: users tab is empty")
    return(NULL)
  }
  
  result <- users |> filter(username_lc == tolower(!!username))
  
  if (nrow(result) == 0) {
    message("    [users] verify_login: no such username")
    return(NULL)
  }
  if (nrow(result) > 1) {
    # Should be impossible (uniqueness enforced at registration), but
    # if it ever happens, fail closed rather than guess.
    message("!!! [users] verify_login: ", nrow(result),
            " rows match one username_lc — failing closed")
    return(NULL)
  }
  
  if (sodium::password_verify(result$password_hash[[1]], password)) {
    message("    [users] verify_login: success for ", result$username[[1]])
    return(result)
  }
  message("    [users] verify_login: password mismatch")
  NULL
}

# Get email for a username
get_email_for_user <- function(username) {
  users <- read_users_tab()
  if (nrow(users) == 0) return(NA_character_)
  result <- users |> filter(username_lc == tolower(!!username))
  if (nrow(result) == 0) return(NA_character_)
  result$email[[1]]
}

# Update a user's password. Returns NULL on success, error message
# string on failure. Used by the change-password modal, and callable
# from the console as an admin reset:
#   update_user_password("someusername", "TheirNewPassword")
#
# Read-then-write pattern (same as update_entry_picks in sheets.R):
# fresh-read the tab to find the row, then range_write the new hash
# into the password_hash cell. Takes effect on the user's next login.
update_user_password <- function(username, new_password) {
  message(">>> update_user_password for username=", username)
  
  err <- validate_password(new_password)
  if (!is.null(err)) return(err)
  
  users <- read_users_tab()
  row_idx <- which(users$username_lc == tolower(username))
  
  if (length(row_idx) == 0) {
    message("    [users] update_user_password: no such username")
    return("No account found with that username.")
  }
  if (length(row_idx) > 1) {
    message("!!! [users] update_user_password: ", length(row_idx),
            " rows match one username_lc — failing closed")
    return("Account lookup failed. Please contact the organiser.")
  }
  
  # +1 for the header row; range_write is 1-indexed with header at row 1.
  sheet_row <- row_idx + 1L
  new_hash  <- sodium::password_store(new_password)
  
  result <- tryCatch({
    # password_hash is column G (username, username_lc, display_name,
    # display_name_lc, email, email_lc, password_hash, ...).
    googlesheets4::range_write(
      ss        = sheet_id(),
      data      = tibble(password_hash = new_hash),
      sheet     = USERS_TAB,
      range     = paste0("G", sheet_row),
      col_names = FALSE
    )
    NULL
  }, error = function(e) {
    message("!!! [users] update_user_password write failed: ",
            conditionMessage(e))
    "Something went wrong updating your password. Please try again."
  })
  
  if (is.null(result)) {
    message("    [users] password updated for ", username,
            " at row ", sheet_row)
  }
  result
}