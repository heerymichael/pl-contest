# Small utility helpers
library(shiny)
library(shinycssloaders)

message(">>> SOURCED R/utils.R at ", format(Sys.time()))

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# Teamstake — third-party payment provider for entry fees.
# Single source of truth for the URL and the link's target/rel attrs.
TEAMSTAKE_URL <- "https://teamstake.com/event/36636/invitation"

teamstake_link <- function(label = "Pay via Teamstake →", class = NULL) {
  tags$a(
    href   = TEAMSTAKE_URL,
    target = "_blank",
    rel    = "noopener noreferrer",
    class  = class,
    label
  )
}

# Loading spinner wrapper.
# Centralised so changing the spinner SVG, dimensions, or any other
# property in this one function updates every spinner across the app.
# Asset lives at www/spinner-loading.svg.
wc_spinner <- function(ui_element) {
  withSpinner(
    ui_element,
    image        = "spinner-loading.svg",
    image.width  = 80,
    image.height = 74
  )
}