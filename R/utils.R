# Small utility helpers (pl-contest)
library(shiny)
library(shinycssloaders)

message(">>> SOURCED R/utils.R at ", format(Sys.time()))

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# Revolut — entry fees are paid by direct Revolut transfer.
# Single source of truth for the URL and the link's target/rel attrs.
#
# Personal revolut.me payment page (verified live 25 Jul 2026).
# Note: Revolut.me does not support presetting an amount in the URL —
# amount presets only exist on single-use in-app payment requests —
# so the rules copy states the \u00a310 explicitly and asks for the
# display name as the payment reference.
REVOLUT_URL <- "https://revolut.me/michaelheery"

payment_link <- function(label = "Pay via Revolut \u2192", class = NULL) {
  tags$a(
    href   = REVOLUT_URL,
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