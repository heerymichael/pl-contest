# Club badge helpers (pl-contest). Replaces the WC app's R/flags.R.
#
# Each of the 20 clubs has an SVG badge at www/logos/{code}.svg, where
# {code} is the club short_code lowercased (ars, avl, ... tot). Unlike
# the WC flags there is no code->filename mapping to maintain — the
# filename IS the short_code.
#
# Function names and signatures parallel the WC flag helpers
# (flag_tag -> badge_tag, flag_html_label -> badge_html_label) so the
# call-site conversion is a pure rename.

message(">>> SOURCED R/badges.R at ", format(Sys.time()))

library(shiny)

# The 20 clubs of 26/27 — used only for the unknown-code diagnostic.
# Source of truth for club data is the `teams` tab; keep in sync if
# promotion/relegation ever changes the set mid-project.
.badge_codes <- c("ARS", "AVL", "BOU", "BRE", "BHA", "CHE", "COV",
                  "CRY", "EVE", "FUL", "HUL", "IPS", "LEE", "LIV",
                  "MCI", "MUN", "NEW", "NFO", "SUN", "TOT")

# Return the badge filename (without .svg) for a short_code, or NA if
# unknown. Vectorised; logs unknown codes for diagnosis.
badge_file <- function(short_code) {
  code <- toupper(short_code)
  ok   <- code %in% .badge_codes
  if (any(!ok)) {
    bad <- unique(short_code[!ok])
    message("    !! badge_file: unknown short_code(s): ",
            paste(bad, collapse = ", "))
  }
  ifelse(ok, tolower(code), NA_character_)
}

# Return an <img> tag for the badge. `size` is "sm" (14px), "md" (18px),
# or "lg" (22px). Returns NULL if the code is unknown so callers can
# skip the badge gracefully.
badge_tag <- function(short_code, size = "sm") {
  f <- badge_file(short_code)
  if (is.na(f)) return(NULL)
  size_class <- switch(size,
                       sm = "badge-icon-sm",
                       md = "badge-icon-md",
                       lg = "badge-icon-lg",
                       "badge-icon-sm")
  tags$img(
    class = paste("badge-icon", size_class),
    src   = paste0("logos/", f, ".svg"),
    alt   = ""
  )
}

# Return an HTML string (badge <img> + club name) for use as a
# selectizeInput choice label. Used by the Club filter dropdown.
badge_html_label <- function(short_code, name) {
  f <- badge_file(short_code)
  if (is.na(f)) return(name)
  sprintf(
    '<img class="badge-icon badge-icon-md" src="logos/%s.svg" alt=""/>%s',
    f, name
  )
}
