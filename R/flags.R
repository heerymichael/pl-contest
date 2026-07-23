# Country flag helpers.
#
# Each of the 48 qualified nations has an SVG flag at www/flags/{iso}.svg,
# where {iso} is the ISO 3166-1 alpha-2 code (lowercase), with two
# special cases for the UK home nations: gb-eng and gb-sct.
#
# Team data uses 3-letter FIFA-style short_codes (ARG, GER, KSA, etc.)
# rather than ISO codes, so this file maintains the mapping from
# short_code -> flag filename, plus helpers for rendering.

message(">>> SOURCED R/flags.R at ", format(Sys.time()))

library(shiny)

# 3-letter short_code -> ISO 2-letter filename (without .svg extension).
# Source of truth: data/teams_clean.csv + www/flags/ directory listing.
.flag_iso_map <- c(
  ALG = "dz", ARG = "ar", AUS = "au", AUT = "at", BEL = "be",
  BIH = "ba", BRA = "br", CAN = "ca", CIV = "ci", COD = "cd",
  COL = "co", CPV = "cv", CRO = "hr", CUW = "cw", CZE = "cz",
  ECU = "ec", EGY = "eg", ENG = "gb-eng", ESP = "es", FRA = "fr",
  GER = "de", GHA = "gh", HAI = "ht", IRN = "ir", IRQ = "iq",
  JOR = "jo", JPN = "jp", KOR = "kr", KSA = "sa", MAR = "ma",
  MEX = "mx", NED = "nl", NOR = "no", NZL = "nz", PAN = "pa",
  PAR = "py", POR = "pt", QAT = "qa", RSA = "za", SCO = "gb-sct",
  SEN = "sn", SUI = "ch", SWE = "se", TUN = "tn", TUR = "tr",
  URU = "uy", USA = "us", UZB = "uz"
)

# Return the flag filename (without .svg) for a short_code, or NA if
# unknown. Vectorised; logs unknown codes for diagnosis.
flag_iso <- function(short_code) {
  iso <- unname(.flag_iso_map[short_code])
  if (any(is.na(iso))) {
    bad <- unique(short_code[is.na(iso)])
    message("    !! flag_iso: unknown short_code(s): ",
            paste(bad, collapse = ", "))
  }
  iso
}

# Return an <img> tag for the flag. `size` is "sm" (14px), "md" (18px),
# or "lg" (22px). Returns NULL if the code is unknown so callers can
# skip the flag gracefully.
flag_tag <- function(short_code, size = "sm") {
  iso <- flag_iso(short_code)
  if (is.na(iso)) return(NULL)
  size_class <- switch(size,
                       sm = "flag-icon-sm",
                       md = "flag-icon-md",
                       lg = "flag-icon-lg",
                       "flag-icon-sm")
  tags$img(
    class = paste("flag-icon", size_class),
    src   = paste0("flags/", iso, ".svg"),
    alt   = ""
  )
}

# Return an HTML string (flag <img> + country name) for use as a
# selectizeInput choice label. Used by the Team filter dropdown.
flag_html_label <- function(short_code, name) {
  iso <- flag_iso(short_code)
  if (is.na(iso)) return(name)
  sprintf(
    '<img class="flag-icon flag-icon-md" src="flags/%s.svg" alt=""/>%s',
    iso, name
  )
}