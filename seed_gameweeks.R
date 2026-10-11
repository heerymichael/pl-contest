# seed_gameweeks.R — create/refresh the `gameweeks` tab on pl-contest-data.
#
# EDIT DRY_RUN IN THIS FILE, not at the console (console assignment is
# overwritten on source()).
#
# Windows are half-open [starts_at, ends_at). Boundary rule:
#   weekend round -> 04:00 Europe/London on the Friday
#   midweek round -> 04:00 Europe/London on the Tuesday
# 04:00 is a dead hour: latest possible finish is a Monday 20:00 kick-off
# (~22:00), and no slate opens before Friday/Tuesday morning.
#
# Datetimes are stored as London-local TEXT so the sheet stays readable and
# matches the other tabs' col_types = "c" convention. Every COMPARISON is
# done in UTC (see to_utc below), so nothing depends on session timezone.
#
# Windows are contiguous by design: ends_at[i] == starts_at[i+1] with no
# gaps, even where no fixture can fall in the tail of a window. A gap would
# mean a rearranged match belonging to no gameweek and scoring for nobody,
# silently.
#
# v2 (11 Oct 2026): DATES UNCHANGED. Status and notes refreshed against the
# Premier League's published TV selections (MW6-12, 2 Dec midweek) and the
# festive fixture release (26 Dec - 7 Jan). Every confirmed kickoff falls
# inside the window it was designed for, and MW20 (Tue 5 Jan 19:30) falls
# after the GW19 hard stop, so nothing from round 20 leaks into GW19.
# GW14-16 (Sat 5/12/19 Dec) stay provisional until their TV picks land
# (due w/c 19 Oct); any Fri/Sat/Sun/Mon slot already sits inside the window.
#
# The FIRST SENTENCE of each note is what gameweek_window_text() shows in
# the app (tooltips/headers), so keep it a short date range (<= 40 chars)
# describing the FIXTURES, not the window. Validation below enforces this.

DRY_RUN <- TRUE
ALLOW_OVERWRITE <- FALSE

library(googlesheets4)

message(">>> SOURCED seed_gameweeks.R (v2) at ", format(Sys.time()))

SHEET_ID <- "1oFdL_U-U4G-3YGXd13Pcb6s4-U01reZwEeVQ-ceUdI8"
TAB_NAME <- "gameweeks"

to_utc <- function(x) {
  as.POSIXct(x, format = "%Y-%m-%d %H:%M", tz = "Europe/London")
}

gw <- data.frame(
  gw          = 1:19,
  starts_at   = c(
    "2026-08-21 04:00", "2026-08-28 04:00", "2026-09-03 04:00",
    "2026-09-12 04:00", "2026-09-18 04:00", "2026-10-10 04:00",
    "2026-10-17 04:00", "2026-10-23 04:00", "2026-10-31 04:00",
    "2026-11-06 04:00", "2026-11-20 04:00", "2026-11-27 04:00",
    "2026-12-01 04:00", "2026-12-04 04:00", "2026-12-11 04:00",
    "2026-12-18 04:00", "2026-12-25 04:00", "2026-12-29 04:00",
    "2027-01-01 04:00"
  ),
  ends_at     = c(
    "2026-08-28 04:00", "2026-09-03 04:00", "2026-09-12 04:00",
    "2026-09-18 04:00", "2026-10-10 04:00", "2026-10-17 04:00",
    "2026-10-23 04:00", "2026-10-31 04:00", "2026-11-06 04:00",
    "2026-11-20 04:00", "2026-11-27 04:00", "2026-12-01 04:00",
    "2026-12-04 04:00", "2026-12-11 04:00", "2026-12-18 04:00",
    "2026-12-25 04:00", "2026-12-29 04:00", "2027-01-01 04:00",
    "2027-01-05 04:00"
  ),
  period      = c(1L, 1L, rep(2L, 17)),
  status      = c(rep("confirmed", 13),          # GW1-13
                  rep("provisional", 3),         # GW14-16: TV picks due w/c 19 Oct
                  rep("confirmed", 3)),          # GW17-19: festive release
  notes       = c(
    "Fri 21 - Mon 24 Aug",
    "Fri 28 - Mon 31 Aug. ENDS EARLY (Thu 3 Sep, not Fri 4): transfer window opens 3 Sep 09:00 and auto-subs run Wed 2 Sep, so GW2 must be closed first. Deliberate override of the boundary rule - do not 'correct'.",
    "Fri 4 - Sun 6 Sep. First match 4 Sep 20:00; transfer window closes 19:00, i.e. 1h before kickoff.",
    "Sat 12 - Mon 14 Sep",
    "Fri 18 - Sun 20 Sep. Widest window in the contest (22 days) - international break follows.",
    "Sat 10 - Mon 12 Oct. Coventry v Newcastle moved to Mon 12 Oct 20:00.",
    "Sat 17 - Mon 19 Oct. Spurs v Coventry moved to Mon 19 Oct 20:00.",
    "Fri 23 - Sun 25 Oct. Ipswich v Forest Fri 23 Oct 20:00. Window spans the DST change (clocks back Sun 25 Oct).",
    "Sat 31 Oct - Mon 2 Nov. Newcastle v Everton moved to Mon 2 Nov 20:00.",
    "Fri 6 - Sun 8 Nov. Everton v Coventry Fri 6 Nov 20:00. International break follows - window runs to Fri 20 Nov.",
    "Sat 21 - Mon 23 Nov. Brentford v Everton moved to Mon 23 Nov 20:00.",
    "Fri 27 - Sun 29 Nov. Forest v Chelsea Fri 27 Nov 20:00. Tue 04:00 close ahead of the midweek round.",
    "Wed 2 Dec midweek round. All ten fixtures 20:00.",
    "Sat 5 Dec listed. Provisional: TV selections for rounds 14-16 due w/c 19 Oct 2026. Any Fri-Mon slot sits inside this window.",
    "Sat 12 Dec listed. Provisional.",
    "Sat 19 Dec listed. Provisional.",
    "Sat 26 - Sun 27 Dec. Last match Palace v Arsenal Sun 27 Dec 19:00; closes Tue 29 Dec 04:00.",
    "Tue 29 - Wed 30 Dec. Midweek round, first kickoffs Tue 29 Dec 19:30.",
    "Fri 1 - Sun 3 Jan 2027. Leeds v Everton Fri 1 Jan 20:00. HARD STOP at Tue 5 Jan 04:00 to enable prompt payout: MW20 begins Tue 5 Jan 19:30 and does not score, nor does any GW19 fixture rearranged beyond this window."
  ),
  stringsAsFactors = FALSE
)

# --- Self-verification (must pass before any write) ---------------------

validate_gw <- function(d) {
  errs <- character(0)
  
  s <- to_utc(d$starts_at)
  e <- to_utc(d$ends_at)
  
  if (any(is.na(s)) || any(is.na(e)))
    errs <- c(errs, "unparseable datetime(s) - check format %Y-%m-%d %H:%M")
  if (!identical(d$gw, seq_len(nrow(d))))
    errs <- c(errs, "gw must be 1..n with no gaps")
  if (any(e <= s, na.rm = TRUE))
    errs <- c(errs, paste0("ends_at <= starts_at at gw ",
                           paste(d$gw[e <= s], collapse = ", ")))
  if (nrow(d) > 1) {
    bad <- which(e[-nrow(d)] != s[-1])
    if (length(bad))
      errs <- c(errs, paste0("non-contiguous: ends_at[", bad,
                             "] != starts_at[", bad + 1, "]", collapse = "; "))
  }
  if (any(diff(d$period) < 0))
    errs <- c(errs, "period must be non-decreasing")
  if (!all(d$period == as.integer(d$period)))
    errs <- c(errs, "period must be integer")
  if (!all(d$status %in% c("confirmed", "provisional")))
    errs <- c(errs, "status must be 'confirmed' or 'provisional'")
  
  # First sentence of each note is the app's display label; keep it short.
  frag <- vapply(strsplit(d$notes, ".", fixed = TRUE),
                 function(x) trimws(x[1]), character(1))
  long <- which(!nzchar(frag) | nchar(frag) > 40)
  if (length(long))
    errs <- c(errs, paste0("notes first sentence missing or > 40 chars at gw ",
                           paste(d$gw[long], collapse = ", "),
                           " (gameweek_window_text would fall back to the raw window)"))
  
  errs
}

errs <- validate_gw(gw)

message("")
print(gw[, c("gw", "starts_at", "ends_at", "period", "status")])
message("")
message(">>> [seed] rows: ", nrow(gw),
        " | periods: ", paste(sort(unique(gw$period)), collapse = ", "),
        " | confirmed: ", sum(gw$status == "confirmed"),
        " | provisional: ", sum(gw$status == "provisional"))
message(">>> [seed] span (UTC): ",
        format(to_utc(gw$starts_at[1]), tz = "UTC", usetz = TRUE), " -> ",
        format(to_utc(gw$ends_at[nrow(gw)]), tz = "UTC", usetz = TRUE))

if (length(errs)) {
  for (x in errs) message(">>> [seed] VALIDATION FAILURE: ", x)
  stop("Validation failed - nothing written.", call. = FALSE)
}
message(">>> [seed] validation PASSED (contiguous, ordered, periods sane, labels short)")

# --- Write --------------------------------------------------------------

existing <- sheet_names(SHEET_ID)
tab_exists <- TAB_NAME %in% existing

if (tab_exists && !isTRUE(ALLOW_OVERWRITE)) {
  message("")
  message(">>> [seed] Tab '", TAB_NAME, "' ALREADY EXISTS. Refusing to ",
          "overwrite - it may carry hand edits (rearranged fixtures, ",
          "tightened provisional dates).")
  message(">>> [seed] Set ALLOW_OVERWRITE <- TRUE in this file to replace it.")
} else if (isTRUE(DRY_RUN)) {
  message("")
  message(">>> [seed] DRY RUN - no write performed. Tab '", TAB_NAME, "' ",
          if (tab_exists) "exists and WOULD BE REPLACED." else "would be created.")
  if (tab_exists) {
    # Show exactly what would change so a refresh never silently moves a date.
    cur <- read_sheet(SHEET_ID, sheet = TAB_NAME, col_types = "c")
    cur <- cur[order(suppressWarnings(as.integer(cur$gw))), , drop = FALSE]
    if (nrow(cur) == nrow(gw)) {
      for (col in c("starts_at", "ends_at", "period", "status", "notes")) {
        diff_rows <- which(as.character(cur[[col]]) != as.character(gw[[col]]))
        if (length(diff_rows))
          message(">>> [seed] would change '", col, "' at gw ",
                  paste(gw$gw[diff_rows], collapse = ", "))
        else
          message(">>> [seed] '", col, "' unchanged")
      }
    } else {
      message(">>> [seed] current tab has ", nrow(cur), " rows vs ", nrow(gw),
              " in this file - full replacement")
    }
  }
  message(">>> [seed] Set DRY_RUN <- FALSE in the file to apply.")
} else {
  write_sheet(gw, ss = SHEET_ID, sheet = TAB_NAME)
  
  chk <- read_sheet(SHEET_ID, sheet = TAB_NAME, col_types = "c")
  stopifnot(
    "verify failed - row count"   = nrow(chk) == nrow(gw),
    "verify failed - starts_at"   = identical(chk$starts_at, gw$starts_at),
    "verify failed - ends_at"     = identical(chk$ends_at,   gw$ends_at),
    "verify failed - period"      = identical(as.integer(chk$period), gw$period)
  )
  message("")
  message(">>> [seed] WRITE VERIFIED - ", nrow(chk), " rows on tab '",
          TAB_NAME, "'")
  message(">>> [seed] Now run check_gameweeks.R to cross-check against fixtures.")
}
