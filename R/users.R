# User registration, login, and lookup helpers (pl-contest).
#
# Sheets-backed: user accounts live in the `users` tab of the contest
# Google Sheet (pl-contest-data). Carried over from the WC app's
# Sheets-backed auth (the SQLite store did not survive shinyapps.io
# instance restarts — ephemeral filesystem).
#
# Schema simplification vs the WC app: five columns only —
#   email, display_name, password_hash, is_admin, created_at
# The parallel username identity and the *_lc shadow columns are gone;
# case-insensitive comparisons happen at read time instead.
#
# Login accepts EITHER email OR display name, disambiguated by the
# presence of "@": identifiers containing "@" are matched against
# email, otherwise against display_name. Display names are prohibited
# from containing "@" (enforced at registration) so the rule is
# unambiguous.
#
# Passwords are sodium hashes stored as text; sodium::password_store()
# output is a plain string and round-trips through a Sheet cell intact.
#
# All reads are fresh (no caching) — auth decisions must never run
# against stale data.

library(dplyr)
library(tibble)
library(sodium)

message(">>> SOURCED R/users.R at ", format(Sys.time()))

USERS_TAB <- "users"

# Explicit column types so every field — especially password_hash —
# always comes back as character rather than being type-guessed.
# Column order: email, display_name, password_hash, is_admin (integer),
# created_at.
.users_col_types <- "cccic"

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
    email         = character(0),
    display_name  = character(0),
    password_hash = character(0),
    is_admin      = integer(0),
    created_at    = character(0)
  )
  googlesheets4::sheet_write(header, ss = sheet_id(), sheet = USERS_TAB)
  message("    [users] created `", USERS_TAB, "` tab with headers")
  invisible(NULL)
}

# Read all users (admin/diagnostic use).
read_all_users <- function() {
  read_users_tab()
}

# Display name validation. Shown on the leaderboard; 2-50 characters
# after trimming, spaces allowed. "@" is prohibited so display names
# can never be mistaken for emails at login.
validate_display_name <- function(display_name) {
  if (is.null(display_name) || trimws(display_name) == "") {
    return("Display name is required.")
  }
  n <- nchar(trimws(display_name))
  if (n < 2 || n > 50) {
    return("Display name must be between 2 and 50 characters.")
  }
  if (grepl("@", display_name, fixed = TRUE)) {
    return("Display name cannot contain the @ character.")
  }
  if (tolower(trimws(display_name)) == "admin") {
    return("That display name is reserved. Please choose another.")
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
# Case-insensitive at read time (no shadow columns in the schema).

# "Mike B" blocks a later "mike b".
display_name_exists <- function(display_name) {
  users <- read_users_tab()
  if (nrow(users) == 0) return(FALSE)
  tolower(trimws(display_name)) %in% tolower(trimws(users$display_name))
}

email_exists <- function(email) {
  users <- read_users_tab()
  if (nrow(users) == 0) return(FALSE)
  tolower(trimws(email)) %in% tolower(trimws(users$email))
}

# Register a new user. Returns NULL on success, error message string
# on failure. Appends one row to the `users` tab.
register_user <- function(email, display_name, password, is_admin = FALSE) {
  err <- validate_email(email);                if (!is.null(err)) return(err)
  err <- validate_display_name(display_name);  if (!is.null(err)) return(err)
  err <- validate_password(password);          if (!is.null(err)) return(err)

  if (email_exists(email)) {
    return("An account with that email already exists.")
  }
  if (display_name_exists(display_name)) {
    return("That display name is already taken. Please choose another.")
  }

  email        <- trimws(email)
  display_name <- trimws(display_name)

  row <- tibble(
    email         = email,
    display_name  = display_name,
    password_hash = sodium::password_store(password),
    is_admin      = as.integer(is_admin),
    created_at    = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )

  result <- tryCatch({
    append_sheet_row(USERS_TAB, row)
    NULL
  }, error = function(e) {
    message("!!! [users] register append failed: ", conditionMessage(e))
    "Something went wrong creating your account. Please try again."
  })

  if (is.null(result)) {
    message("    [users] registered user ", display_name, " <", email, ">")
  }
  result
}

# Internal: resolve a login identifier to matching user rows.
# Identifiers containing "@" are matched against email, everything
# else against display_name. Case-insensitive, whitespace-trimmed.
.match_identifier <- function(users, identifier) {
  ident <- tolower(trimws(identifier))
  if (grepl("@", ident, fixed = TRUE)) {
    users |> filter(tolower(trimws(email)) == ident)
  } else {
    users |> filter(tolower(trimws(display_name)) == ident)
  }
}

# Verify login credentials. `identifier` is an email address or a
# display name. Returns the user row (1-row tibble) on success, NULL
# on failure.
verify_login <- function(identifier, password) {
  if (is.null(identifier) || is.null(password) ||
      identifier == "" || password == "") {
    return(NULL)
  }

  users <- read_users_tab()
  if (nrow(users) == 0) {
    message("    [users] verify_login: users tab is empty")
    return(NULL)
  }

  result <- .match_identifier(users, identifier)

  if (nrow(result) == 0) {
    message("    [users] verify_login: no such account")
    return(NULL)
  }
  if (nrow(result) > 1) {
    # Should be impossible (uniqueness enforced at registration), but
    # if it ever happens, fail closed rather than guess.
    message("!!! [users] verify_login: ", nrow(result),
            " rows match one identifier — failing closed")
    return(NULL)
  }

  if (sodium::password_verify(result$password_hash[[1]], password)) {
    message("    [users] verify_login: success for ",
            result$display_name[[1]])
    return(result)
  }
  message("    [users] verify_login: password mismatch")
  NULL
}

# Get email for a display name (or pass an email through, validated
# against the tab). Returns NA_character_ if no account matches.
get_email_for_user <- function(identifier) {
  users <- read_users_tab()
  if (nrow(users) == 0) return(NA_character_)
  result <- .match_identifier(users, identifier)
  if (nrow(result) != 1) return(NA_character_)
  result$email[[1]]
}

# Update a user's password. Returns NULL on success, error message
# string on failure. Used by the change-password modal, and callable
# from the console as an admin reset:
#   update_user_password("their@email.com", "TheirNewPassword")
#   update_user_password("Their Display Name", "TheirNewPassword")
#
# Read-then-write pattern: fresh-read the tab to find the row, then
# range_write the new hash into the password_hash cell. Takes effect
# on the user's next login.
update_user_password <- function(identifier, new_password) {
  message(">>> update_user_password for identifier=", identifier)

  err <- validate_password(new_password)
  if (!is.null(err)) return(err)

  users   <- read_users_tab()
  matched <- .match_identifier(users, identifier)
  row_idx <- which(
    tolower(trimws(users$email)) %in% tolower(trimws(matched$email))
  )

  if (length(row_idx) == 0) {
    message("    [users] update_user_password: no such account")
    return("No account found with that email or display name.")
  }
  if (length(row_idx) > 1) {
    message("!!! [users] update_user_password: ", length(row_idx),
            " rows match one identifier — failing closed")
    return("Account lookup failed. Please contact the organiser.")
  }

  # +1 for the header row; range_write is 1-indexed with header at row 1.
  sheet_row <- row_idx + 1L
  new_hash  <- sodium::password_store(new_password)

  result <- tryCatch({
    # password_hash is column C (email, display_name, password_hash,
    # is_admin, created_at).
    googlesheets4::range_write(
      ss        = sheet_id(),
      data      = tibble(password_hash = new_hash),
      sheet     = USERS_TAB,
      range     = paste0("C", sheet_row),
      col_names = FALSE
    )
    NULL
  }, error = function(e) {
    message("!!! [users] update_user_password write failed: ",
            conditionMessage(e))
    "Something went wrong updating your password. Please try again."
  })

  if (is.null(result)) {
    message("    [users] password updated for ",
            matched$display_name[[1]], " at row ", sheet_row)
  }
  result
}
