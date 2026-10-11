# R/gameweeks.R ----------------------------------------------------------------
# The gameweek dimension: the bridge between a match kickoff and the
# contest's gameweek numbering.
#
# The `gameweeks` tab on pl-contest-data holds 19 half-open windows
# [starts_at, ends_at) stored as Europe/London TEXT, seeded by
# seed_gameweeks.R and validated by check_gameweeks.R. This file is the
# app-side reader for that tab plus the assignment functions that hang
# off it.
#
# THE WINDOW IS AUTHORITATIVE, NOT THE OFFICIAL MATCHDAY. A match
# rearranged out of its official round scores in whatever window its
# kickoff actually falls in. That is a deliberate contest rule (a fixture
# moved to midweek shouldn't score for a round that finished a fortnight
# ago), and it is why gameweeks are computed here from `utc_date` rather
# than trusted from an ingest-time column.
#
# All comparison happens in absolute time. Sheet values are parsed as
# Europe/London (so the 04:00 boundaries survive the October DST change);
# stat timestamps are parsed as UTC. POSIXct comparison is absolute, so
# nothing depends on session timezone.
#
# Matches falling in NO window score for nobody. That is correct for
# anything past the GW19 hard stop (Tue 5 Jan 04:00, set so payout isn't
# held hostage to a rearranged fixture in March) and a red flag anywhere
# else — so it is logged loudly rather than silently dropped.
#
# Main entry points:
#   get_gameweeks()                    -> the tab, parsed and validated
#   get_gameweeks_cached()             -> same, 5-min TTL
#   gameweek_of_kickoff(k)             -> vector of gw numbers (NA if none)
#   assign_gameweeks(stats)            -> stats + a `gameweek` column
#   gameweeks_with_stats(scored)       -> which gws have any scored rows
#   current_gameweek()                 -> the window we're inside now
#   gameweek_is_closed(g)              -> has this window ended
#   gameweek_window_text(g)            -> "Fri 21 - Mon 24 Aug" for display
#   gameweeks_selftest()               -> console check, no app required
# ------------------------------------------------------------------------------

message(">>> SOURCED R/gameweeks.R at ", format(Sys.time()))

library(googlesheets4)

GW_TAB <- "gameweeks"

# --- Datetime parsing ---------------------------------------------------

# Sheet values: London-local text, "%Y-%m-%d %H:%M" as written by
# seed_gameweeks.R. Seconds tolerated in case a value is ever hand-edited
# through the Sheets UI, which likes to add them.
.gw_parse_london <- function(x) {
  s   <- trimws(as.character(x))
  out <- as.POSIXct(s, format = "%Y-%m-%d %H:%M", tz = "Europe/London")
  need <- is.na(out) & nzchar(s) & !is.na(s)
  if (any(need)) {
    out[need] <- as.POSIXct(s[need], format = "%Y-%m-%d %H:%M:%S",
                            tz = "Europe/London")
  }
  out
}

# Stat/fixture timestamps: UTC, in whatever shape the API and the Sheet
# round-trip produces — "2026-08-21 19:00:00", ISO with a T separator, a
# trailing Z, or an explicit +00:00 offset.
#
# A DATE-ONLY value falls back to 12:00 UTC, not midnight. Midnight would
# land before the 04:00 boundary and silently push the match into the
# PREVIOUS gameweek; noon is inside the correct window for every kickoff
# slot the Premier League uses. It is a fallback, not a design: a stats
# row with no time on it should be chased down at the ingest.
.gw_parse_utc <- function(x) {
  s <- trimws(as.character(x))
  s <- sub("T", " ", s, fixed = TRUE)
  s <- sub("Z$", "", s)
  s <- sub("\\+00:?00$", "", s)
  s <- trimws(s)
  
  out  <- as.POSIXct(s, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  need <- is.na(out) & nzchar(s) & !is.na(s)
  if (any(need)) {
    out[need] <- as.POSIXct(s[need], format = "%Y-%m-%d %H:%M", tz = "UTC")
  }
  need <- is.na(out) & nzchar(s) & !is.na(s)
  if (any(need)) {
    out[need] <- as.POSIXct(paste0(s[need], " 12:00:00"),
                            format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  }
  out
}

# --- Reading the tab ----------------------------------------------------

# Fresh read of the gameweeks tab, parsed into usable columns:
#   gw (integer), starts_at / ends_at (original text), starts_utc /
#   ends_utc (POSIXct), period (integer), status, notes
#
# Structural problems are MESSAGED, not thrown: a malformed gameweeks tab
# should degrade the leaderboard to "no gameweek views", never take the
# app down. check_gameweeks.R is the place that fails loudly.
#
# Returns a zero-row frame with the right columns if the tab is missing
# or unreadable, so every caller can just check nrow().
get_gameweeks <- function() {
  message(">>> get_gameweeks")
  
  empty <- data.frame(
    gw = integer(0), starts_at = character(0), ends_at = character(0),
    starts_utc = as.POSIXct(character(0), tz = "UTC"),
    ends_utc   = as.POSIXct(character(0), tz = "UTC"),
    period = integer(0), status = character(0), notes = character(0),
    stringsAsFactors = FALSE
  )
  
  raw <- tryCatch(
    read_sheet(sheet_id(), sheet = GW_TAB, col_types = "c"),
    error = function(e) {
      message("    get_gameweeks read FAILED: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(raw) || nrow(raw) == 0) {
    message("    get_gameweeks: no rows — gameweek views unavailable")
    return(empty)
  }
  
  for (col in c("gw", "starts_at", "ends_at")) {
    if (!col %in% names(raw)) {
      message("    get_gameweeks: tab is missing column '", col,
              "' — gameweek views unavailable")
      return(empty)
    }
  }
  
  d <- data.frame(
    gw         = suppressWarnings(as.integer(raw$gw)),
    starts_at  = as.character(raw$starts_at),
    ends_at    = as.character(raw$ends_at),
    period     = if ("period" %in% names(raw))
      suppressWarnings(as.integer(raw$period)) else NA_integer_,
    status     = if ("status" %in% names(raw))
      as.character(raw$status) else NA_character_,
    notes      = if ("notes"  %in% names(raw))
      as.character(raw$notes)  else NA_character_,
    stringsAsFactors = FALSE
  )
  d$starts_utc <- .gw_parse_london(d$starts_at)
  d$ends_utc   <- .gw_parse_london(d$ends_at)
  d <- d[order(d$gw), , drop = FALSE]
  rownames(d) <- NULL
  
  # Structural sanity — surfaced in the log, never fatal here.
  if (any(is.na(d$gw)) || any(is.na(d$starts_utc)) || any(is.na(d$ends_utc))) {
    message("    !!! get_gameweeks: unparseable gw/datetime on ",
            sum(is.na(d$gw) | is.na(d$starts_utc) | is.na(d$ends_utc)),
            " row(s) — those windows will never match a kickoff")
  }
  if (any(d$ends_utc <= d$starts_utc, na.rm = TRUE)) {
    message("    !!! get_gameweeks: non-positive window at gw ",
            paste(d$gw[d$ends_utc <= d$starts_utc], collapse = ", "))
  }
  if (nrow(d) > 1) {
    bad <- which(d$ends_utc[-nrow(d)] != d$starts_utc[-1])
    if (length(bad) > 0) {
      message("    !!! get_gameweeks: gap/overlap between gw ",
              paste(d$gw[bad], "and", d$gw[bad + 1], collapse = "; "),
              " — a match in the gap scores for nobody. Run check_gameweeks.R")
    }
  }
  
  message("    get_gameweeks: ", nrow(d), " windows, gw ",
          min(d$gw, na.rm = TRUE), "-", max(d$gw, na.rm = TRUE),
          ", periods ", paste(sort(unique(d$period[!is.na(d$period)])),
                              collapse = "/"))
  d
}

# Memoised read on the same 5-minute store as contest_config and
# player_match_stats, so a hand-edit to the tab (a rearranged fixture
# tightening a window) reaches the live board within five minutes with
# no redeploy.
#
# The store is built lazily so this file can be sourced in isolation at
# the console — before, after, or without R/cache.R — and still work.
get_gameweeks_cached <- local({
  underlying <- NULL
  function() {
    if (is.null(underlying)) {
      store <- if (exists(".cache_store", envir = globalenv())) {
        get(".cache_store", envir = globalenv())
      } else {
        cachem::cache_mem(max_age = 5 * 60)
      }
      underlying <<- memoise::memoise(
        function() {
          message("    [cache] MISS gameweeks — fetching from Sheets")
          get_gameweeks()
        },
        cache = store
      )
    }
    underlying()
  }
})

# --- Assignment ---------------------------------------------------------

# Map kickoffs to gameweek numbers. `kickoff` may be POSIXct or any of
# the text shapes .gw_parse_utc() understands.
#
# Returns an integer vector the same length as `kickoff`; NA where the
# timestamp is unparseable or falls outside every window.
gameweek_of_kickoff <- function(kickoff, gw_tbl = get_gameweeks_cached()) {
  n <- length(kickoff)
  if (n == 0) return(integer(0))
  if (is.null(gw_tbl) || nrow(gw_tbl) == 0) return(rep(NA_integer_, n))
  
  k <- if (inherits(kickoff, "POSIXct")) kickoff else .gw_parse_utc(kickoff)
  
  s  <- gw_tbl$starts_utc
  e  <- gw_tbl$ends_utc
  g  <- gw_tbl$gw
  ok <- !is.na(s) & !is.na(e) & !is.na(g)
  s  <- s[ok]; e <- e[ok]; g <- g[ok]
  if (length(g) == 0) return(rep(NA_integer_, n))
  
  vapply(seq_len(n), function(i) {
    if (is.na(k[i])) return(NA_integer_)
    hit <- which(k[i] >= s & k[i] < e)
    if (length(hit) == 1L) g[hit] else NA_integer_
  }, integer(1))
}

# Add a `gameweek` column to a player_match_stats frame (or to the output
# of score_player_match_stats(), which carries the same timestamp column
# through).
#
# The kickoff column is auto-detected so this survives an ingest that
# names it kickoff_utc rather than utc_date. If the incoming frame
# already carries a gameweek-ish column from the ingest it is preserved
# as `matchday` for reference and NOT used for scoring — see the header
# note on why the window wins.
assign_gameweeks <- function(stats, gw_tbl = get_gameweeks_cached()) {
  if (is.null(stats) || nrow(stats) == 0) {
    if (!is.null(stats)) stats$gameweek <- integer(0)
    return(stats)
  }
  
  ko_col <- intersect(c("utc_date", "kickoff_utc", "kickoff", "match_date"),
                      names(stats))
  if (length(ko_col) == 0) {
    message("!!! assign_gameweeks: no kickoff column found (looked for ",
            "utc_date / kickoff_utc / kickoff / match_date) — every row ",
            "gets gameweek NA and nothing will score")
    stats$gameweek <- NA_integer_
    return(stats)
  }
  ko_col <- ko_col[1]
  
  # Preserve an ingest-supplied round number under a name that can't be
  # confused with the contest's own gameweek.
  if ("gameweek" %in% names(stats) && !"matchday" %in% names(stats)) {
    stats$matchday <- stats$gameweek
  }
  
  stats$gameweek <- gameweek_of_kickoff(stats[[ko_col]], gw_tbl)
  
  n_rows <- nrow(stats)
  n_na   <- sum(is.na(stats$gameweek))
  matches <- if ("match_id" %in% names(stats)) {
    unique(stats$match_id[is.na(stats$gameweek)])
  } else character(0)
  
  message(">>> assign_gameweeks: ", n_rows, " rows via '", ko_col, "' -> gw ",
          paste(sort(unique(stats$gameweek[!is.na(stats$gameweek)])),
                collapse = ", "))
  if (n_na > 0) {
    message("    !!! ", n_na, " row(s) in NO gameweek window",
            if (length(matches) > 0)
              paste0(" across ", length(matches), " match(es): ",
                     paste(utils::head(matches, 5), collapse = ", "),
                     if (length(matches) > 5) " ..." else "")
            else "",
            " — these score for nobody. Expected only past the GW",
            if (nrow(gw_tbl) > 0) max(gw_tbl$gw, na.rm = TRUE) else "19",
            " hard stop.")
  }
  
  # Rearrangement visibility: official round disagreeing with the window
  # is correct behaviour, but should never be a surprise.
  if ("matchday" %in% names(stats)) {
    md    <- suppressWarnings(as.integer(stats$matchday))
    moved <- !is.na(md) & !is.na(stats$gameweek) & md != stats$gameweek
    if (any(moved)) {
      pairs <- unique(paste0("MD", md[moved], "->GW", stats$gameweek[moved]))
      message("    note: ", sum(moved), " row(s) rearranged out of their ",
              "official round (", paste(pairs, collapse = ", "),
              ") — they score in the window, by design")
    }
  }
  
  stats
}

# --- Queries ------------------------------------------------------------

# Gameweeks that have at least one scored row. This is what lights up the
# leaderboard's gameweek pills: a gameweek becomes selectable as soon as
# its first match is ingested, showing points to date for that round,
# not once the round is complete.
gameweeks_with_stats <- function(scored) {
  if (is.null(scored) || nrow(scored) == 0) return(integer(0))
  if (!"gameweek" %in% names(scored)) return(integer(0))
  g <- suppressWarnings(as.integer(scored$gameweek))
  sort(unique(g[!is.na(g)]))
}

# The gameweek whose window contains `now`. NA before GW1 opens, between
# the hard stop and never, or if the tab is unavailable.
current_gameweek <- function(gw_tbl = get_gameweeks_cached(),
                             now = Sys.time()) {
  gameweek_of_kickoff(now, gw_tbl)
}

# Has this gameweek's window ended? A closed gameweek's points are final
# (barring a scoring correction or a late stats ingest); an open one is
# still accumulating.
gameweek_is_closed <- function(g, gw_tbl = get_gameweeks_cached(),
                               now = Sys.time()) {
  if (is.null(gw_tbl) || nrow(gw_tbl) == 0) return(FALSE)
  row <- gw_tbl[!is.na(gw_tbl$gw) & gw_tbl$gw == g, , drop = FALSE]
  if (nrow(row) != 1 || is.na(row$ends_utc[1])) return(FALSE)
  as.numeric(now) >= as.numeric(row$ends_utc[1])
}

# The period a gameweek belongs to (1 = GW1-2, 2 = GW3-19 after the
# September transfer window). NA if unknown — callers should treat that
# as "no period filtering", not as "no picks".
gameweek_period <- function(g, gw_tbl = get_gameweeks_cached()) {
  if (is.null(gw_tbl) || nrow(gw_tbl) == 0) return(NA_integer_)
  row <- gw_tbl[!is.na(gw_tbl$gw) & gw_tbl$gw == g, , drop = FALSE]
  if (nrow(row) != 1) return(NA_integer_)
  as.integer(row$period[1])
}

# Human-readable date range for a gameweek, for tooltips and headers.
# Prefers the hand-written notes fragment ("Fri 21 - Mon 24 Aug") since
# that describes the FIXTURES; falls back to the window itself, which is
# wider than the fixtures by design.
gameweek_window_text <- function(g, gw_tbl = get_gameweeks_cached()) {
  if (is.null(gw_tbl) || nrow(gw_tbl) == 0) return("")
  row <- gw_tbl[!is.na(gw_tbl$gw) & gw_tbl$gw == g, , drop = FALSE]
  if (nrow(row) != 1) return("")
  
  nt <- trimws(as.character(row$notes[1]))
  if (!is.na(nt) && nzchar(nt)) {
    frag <- trimws(strsplit(nt, ".", fixed = TRUE)[[1]][1])
    if (nzchar(frag) && nchar(frag) <= 40) return(frag)
  }
  
  if (is.na(row$starts_utc[1]) || is.na(row$ends_utc[1])) return("")
  paste0(format(row$starts_utc[1], "%d %b", tz = "Europe/London"), " - ",
         format(row$ends_utc[1] - 86400, "%d %b", tz = "Europe/London"))
}

# --- Self-test ----------------------------------------------------------

# Console check with no app and no scoring required. Runs THIS FILE's
# assignment path against the tidy fixture list from probe_pl_2627.R, so
# a regression here is caught before the leaderboard depends on it.
#
# Complements check_gameweeks.R rather than duplicating it: that script
# validates the TAB, this one validates the CODE that reads it.
#
#   source("R/sheets.R"); gs_auth(); source("R/gameweeks.R")
#   gameweeks_selftest()
gameweeks_selftest <- function(
    fixtures_csv = "data/api_samples_pl/2627_fixtures_tidy.csv") {
  
  message("")
  message(">>> [gw selftest] reading gameweeks tab")
  d <- get_gameweeks()
  if (nrow(d) == 0) {
    message(">>> [gw selftest] FAILED: no gameweek windows readable")
    return(invisible(FALSE))
  }
  
  message(">>> [gw selftest] ", nrow(d), " windows, ",
          format(d$starts_utc[1], tz = "UTC", usetz = TRUE), " -> ",
          format(d$ends_utc[nrow(d)], tz = "UTC", usetz = TRUE))
  message(">>> [gw selftest] current gameweek: ",
          {cg <- current_gameweek(d); if (is.na(cg)) "none (outside all windows)" else cg})
  
  if (!file.exists(fixtures_csv)) {
    message(">>> [gw selftest] SKIPPED fixture pass: ", fixtures_csv,
            " not found. Run probe_pl_2627.R first.")
    return(invisible(TRUE))
  }
  
  fx <- read.csv(fixtures_csv, stringsAsFactors = FALSE, colClasses = "character")
  fx <- fx[nzchar(fx$kickoff_utc), , drop = FALSE]
  
  # Exercise the real path: rename to the ingest's column name and run
  # assign_gameweeks() exactly as the leaderboard will.
  fx$utc_date <- fx$kickoff_utc
  fx$matchday <- suppressWarnings(as.integer(fx$matchday))
  fx <- assign_gameweeks(fx, d)
  
  message("")
  message(">>> [gw selftest] matches per window:")
  for (g in d$gw) {
    n <- sum(!is.na(fx$gameweek) & fx$gameweek == g)
    note <- if (n == 0L) "  <-- none loaded yet"
    else if (n != 10L) "  <-- NOT 10: check boundary or double/blank"
    else ""
    message("    gw ", sprintf("%2d", g), ": ", sprintf("%2d", n), note)
  }
  
  ok_rows <- sum(!is.na(fx$gameweek))
  message("")
  message(">>> [gw selftest] ", ok_rows, " of ", nrow(fx),
          " fixtures assigned to a window")
  message(">>> [gw selftest] gameweeks with fixtures: ",
          paste(sort(unique(fx$gameweek[!is.na(fx$gameweek)])), collapse = ", "))
  
  # Round-trip the boundary itself: one second before a window opens must
  # land in the previous gameweek, and the boundary instant in the new one.
  if (nrow(d) > 1) {
    b      <- d$starts_utc[2]
    before <- gameweek_of_kickoff(b - 1, d)
    at     <- gameweek_of_kickoff(b,     d)
    if (identical(before, d$gw[1]) && identical(at, d$gw[2])) {
      message(">>> [gw selftest] ok: half-open boundary correct at ",
              d$starts_at[2], " (", before, " -> ", at, ")")
    } else {
      message(">>> [gw selftest] FAIL: boundary at ", d$starts_at[2],
              " gave ", before, " -> ", at, " (expected ", d$gw[1],
              " -> ", d$gw[2], ")")
      return(invisible(FALSE))
    }
  }
  
  message(">>> [gw selftest] ===== PASSED =====")
  invisible(TRUE)
}