# check_gameweeks.R — read-only validation of the `gameweeks` tab. (v2)
#
# Makes no writes of any kind. Safe to run at any time.
#
# Two layers:
#   1. STRUCTURE  - contiguity, ordering, period sanity. Cheap, always runs.
#   2. FIXTURES   - cross-check against the tidy fixture list from
#                   probe_pl_2627.R. This is the layer with teeth: structure
#                   alone is satisfied by 19 windows uniformly a week wrong.
#
# All datetime comparison happens in UTC. Sheet values are London-local text
# and are converted on read; the API's kickoff_utc is already UTC.
#
# v2 changes:
#   - removed dead `covered` variable (computed, never used)
#   - period summary no longer relies on unique() and tapply() agreeing on
#     ordering (they don't, if a period ever appears out of sequence)
#   - zero-fixture note reworded: "no fixtures loaded yet" was true in
#     August but would wrongly reassure once all matchdays are loaded

library(googlesheets4)

message(">>> SOURCED check_gameweeks.R (v2) at ", format(Sys.time()))

SHEET_ID     <- "1oFdL_U-U4G-3YGXd13Pcb6s4-U01reZwEeVQ-ceUdI8"
TAB_NAME     <- "gameweeks"
FIXTURES_CSV <- "data/api_samples_pl/2627_fixtures_tidy.csv"

to_utc <- function(x) {
  as.POSIXct(x, format = "%Y-%m-%d %H:%M", tz = "Europe/London")
}

fail <- 0L
flag <- function(...) { fail <<- fail + 1L; message(">>> [check] FAIL: ", ...) }
ok   <- function(...) message(">>> [check] ok: ", ...)

# --- 1. Structure -------------------------------------------------------

d <- read_sheet(SHEET_ID, sheet = TAB_NAME, col_types = "c")
message(">>> [check] read ", nrow(d), " rows from tab '", TAB_NAME, "'")

for (col in c("gw", "starts_at", "ends_at", "period")) {
  if (!col %in% names(d)) flag("missing column '", col, "'")
}
if (fail > 0L) stop("Schema incomplete - stopping.", call. = FALSE)

d$gw_i   <- suppressWarnings(as.integer(d$gw))
d$per_i  <- suppressWarnings(as.integer(d$period))
d        <- d[order(d$gw_i), ]
s        <- to_utc(d$starts_at)
e        <- to_utc(d$ends_at)

if (any(is.na(s)) || any(is.na(e))) {
  flag("unparseable datetime(s) at gw ",
       paste(d$gw_i[is.na(s) | is.na(e)], collapse = ", "))
} else ok("all datetimes parse")

if (!identical(d$gw_i, seq_len(nrow(d)))) {
  flag("gw is not 1..", nrow(d), " contiguous")
} else ok("gw numbering contiguous 1..", nrow(d))

if (any(e <= s, na.rm = TRUE)) {
  flag("ends_at <= starts_at at gw ", paste(d$gw_i[e <= s], collapse = ", "))
} else ok("every window has positive duration")

if (nrow(d) > 1) {
  bad <- which(e[-nrow(d)] != s[-1])
  if (length(bad)) {
    for (i in bad)
      flag("gap/overlap between gw ", d$gw_i[i], " and gw ", d$gw_i[i + 1],
           " (", d$ends_at[i], " != ", d$starts_at[i + 1], ")")
  } else ok("windows contiguous - no gaps, no overlaps")
}

if (any(is.na(d$per_i))) {
  flag("non-integer period value(s)")
} else if (any(diff(d$per_i) < 0)) {
  flag("period decreases at gw ", paste(d$gw_i[-1][diff(d$per_i) < 0],
                                        collapse = ", "))
} else {
  rng <- tapply(d$gw_i, d$per_i, function(x) paste0(min(x), "-", max(x)))
  ok("period non-decreasing: ",
     paste(sprintf("p%s=gw%s", names(rng), rng), collapse = ", "))
}

# --- 2. Fixture cross-check ---------------------------------------------

if (!file.exists(FIXTURES_CSV)) {
  message(">>> [check] SKIPPED fixture cross-check: ", FIXTURES_CSV,
          " not found. Run probe_pl_2627.R first.")
} else {
  fx <- read.csv(FIXTURES_CSV, stringsAsFactors = FALSE, colClasses = "character")
  fx <- fx[nzchar(fx$kickoff_utc), , drop = FALSE]
  ko <- as.POSIXct(fx$kickoff_utc, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  fx <- fx[!is.na(ko), , drop = FALSE]
  ko <- ko[!is.na(ko)]
  
  message(">>> [check] fixture list: ", nrow(fx), " matches with kickoffs, ",
          "official matchdays ", min(as.integer(fx$matchday), na.rm = TRUE),
          "-", max(as.integer(fx$matchday), na.rm = TRUE))
  
  # Assign each fixture to a window: [starts_at, ends_at)
  assign_gw <- function(k) {
    hit <- which(k >= s & k < e)
    if (length(hit) == 1L) d$gw_i[hit] else NA_integer_
  }
  fx$window_gw <- vapply(ko, assign_gw, integer(1))
  
  # 2a. Orphans - matches in no window. With GW19 hard-stopped this is
  #     expected for later rounds, but must never be silent.
  orphan  <- is.na(fx$window_gw)
  in_span <- ko >= min(s) & ko < max(e)
  if (any(orphan & in_span)) {
    for (i in which(orphan & in_span))
      flag("match ", fx$match_id[i], " (", fx$home[i], " v ", fx$away[i],
           ", ", fx$kickoff_utc[i], " UTC) falls in NO window despite being ",
           "inside the contest span")
  } else ok("no orphaned matches inside the contest span")
  
  if (any(orphan & !in_span))
    message(">>> [check] note: ", sum(orphan & !in_span),
            " match(es) outside the GW1-", max(d$gw_i),
            " span entirely (expected - later rounds).")
  
  # 2b. Counts per window. Not-10 is not an error (doubles and blanks are
  #     legitimate) but it is the signature of a misplaced boundary.
  cnt <- table(factor(fx$window_gw[!orphan], levels = d$gw_i))
  message(">>> [check] matches per window:")
  for (g in d$gw_i) {
    n <- as.integer(cnt[as.character(g)])
    note <- if (n == 0L) "  <-- no fixtures in this window"
    else if (n != 10L) "  <-- NOT 10: verify boundary or genuine double/blank"
    else ""
    message("    gw ", sprintf("%2d", g), ": ", sprintf("%2d", n), note)
  }
  
  # 2c. Rearrangements - official matchday disagrees with date window.
  #     Expected and correct behaviour, but should be visible.
  md    <- suppressWarnings(as.integer(fx$matchday))
  moved <- !orphan & !is.na(md) & md != fx$window_gw
  if (any(moved)) {
    message(">>> [check] REARRANGEMENTS (official round != date window - ",
            "these score in the window, by design):")
    for (i in which(moved))
      message("    ", fx$match_id[i], "  ", fx$home[i], " v ", fx$away[i],
              "  official MD", md[i], " -> window gw", fx$window_gw[i],
              "  (", fx$kickoff_utc[i], " UTC)")
  } else ok("no rearrangements - every match sits in its official round's window")
}

message("")
if (fail == 0L) {
  message(">>> [check] ===== ALL CHECKS PASSED =====")
} else {
  message(">>> [check] ===== ", fail, " FAILURE(S) - see above =====")
}