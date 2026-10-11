# R/bestball.R -----------------------------------------------------------------
# Bestball engine for the PL contest.
#
# Each entry rosters 15 players (2 GK / 5 DEF / 5 MID / 3 FWD, max one
# per club). Every gameweek the app automatically selects the entry's
# optimal XI from any valid formation:
#   exactly 1 GK, 3-5 DEF, 2-5 MID, 1-3 FWD, 11 total.
# Entry total = sum of optimal-XI points across all gameweeks. No
# lineups are set by users; there is nothing to submit weekly.
#
# Double gameweeks: a player's points for a gameweek are the SUM across
# all their fixtures in that gameweek (banked before XI selection).
# Players with no fixture / no appearance score 0 but remain selectable
# (they only appear in the XI if the position minima force it).
#
# THE POSITION TABLE COMES FROM THE ROSTER, NOT FROM THE STATS. Every
# rostered player must be in the selection pool at 0 points even when
# they haven't kicked a ball, because the minima (1 GK / 3 DEF / 2 MID /
# 1 FWD) have to be satisfiable from the first match of the season.
# Deriving positions from scored rows only — as v1 did — means that on a
# mid-gameweek board, where an entry might have six appearances so far,
# the pool cannot field a legal XI and the entry scores zero. Positions
# are resolved from the players table, falling back to the roster's own
# `slot` column.
#
# PERIODS: after the September transfer window an entry has TWO sets of
# 15 picks on roster_picks, tagged period 1 (GW1-2) and period 2
# (GW3-19). `gw_period` maps gameweek -> period so each gameweek scores
# against the squad actually held at the time. The filter is deliberately
# tolerant: if a period has no rows for an entry (period column unfilled,
# or the period-2 baseline not yet seeded) the full pick set is used
# rather than an empty roster. A missing period must never silently zero
# someone's score.
#
# TIE-BREAKS use FanTeam salary, descending. Early in a gameweek most of
# a squad sits on 0, so which eleven get marked as the XI is otherwise
# decided by player_id — an arbitrary artefact of pool ingest order, which
# looks like the app choosing badly. Salary is the pool's own view of who
# the better player is, so a tied XI reads as the squad you would expect.
# This changes DISPLAY ONLY: a tie is by definition between equal-points
# players, so the XI total is identical whichever is picked. player_id
# remains the final fallback, so selection stays deterministic.
#
# Pure functions, same contract as R/scoring.R: no side effects, no
# Sheets reads. Consumes score_player_match_stats() output that has been
# through assign_gameweeks() in R/gameweeks.R.
#
# Main entry points:
#   select_best_xi(pool)                         -> one gameweek, one entry
#   bestball_gameweek_points(scored, picks, ...) -> entry x gameweek totals
#   bestball_leaderboard(scored, picks, entries) -> the contest leaderboard
#   bestball_entry_xi(scored, picks, eid, gw)    -> one squad, one gameweek,
#                                                   with in_xi marked
# ------------------------------------------------------------------------------

message(">>> SOURCED R/bestball.R at ", format(Sys.time()))

# Formation bounds. Selection = exactly BB_MIN per position, then the 4
# remaining slots greedily filled by points subject to BB_MAX. With the
# minima fixed, greedy fill respecting per-position caps is optimal
# (independent per-position caps — no interaction between positions).
BB_MIN <- c(GK = 1, DEF = 3, MID = 2, FWD = 1)
BB_MAX <- c(GK = 1, DEF = 5, MID = 5, FWD = 3)
BB_XI  <- 11L

BB_POSITIONS <- c("GK", "DEF", "MID", "FWD")

# pool: data frame with player_id, position, points — one row per
# rostered player for ONE entry in ONE gameweek (points already summed
# across the gameweek's fixtures; non-appearing players present with 0).
#
# Returns the pool with a logical `in_xi` column marking the optimal XI.
# Stops if the pool cannot field a valid XI (indicates a broken roster —
# every legal 2/5/5/3 roster can always field one).
select_best_xi <- function(pool) {
  pool$points   <- ifelse(is.na(pool$points), 0, pool$points)
  pool$position <- as.character(pool$position)
  
  bad <- setdiff(unique(pool$position), names(BB_MIN))
  if (length(bad) > 0) {
    stop("select_best_xi: unknown position(s): ", paste(bad, collapse = ", "))
  }
  
  # Tie-break value, higher wins. Callers supply FanTeam salary; absent or
  # unparseable prices sink to the bottom of a tie rather than erroring.
  if (!"tiebreak" %in% names(pool)) pool$tiebreak <- 0
  tb <- suppressWarnings(as.numeric(pool$tiebreak))
  pool$tiebreak <- ifelse(is.na(tb), 0, tb)
  
  # Order within position by points desc, then salary desc, then player_id
  # so selection is fully deterministic.
  pool <- pool[order(pool$position, -pool$points, -pool$tiebreak,
                     pool$player_id), ]
  pool$pos_rank <- ave(seq_along(pool$points), pool$position,
                       FUN = seq_along)
  
  have <- table(factor(pool$position, levels = names(BB_MIN)))
  if (any(have < BB_MIN)) {
    stop("select_best_xi: pool cannot satisfy position minima (",
         paste(names(BB_MIN), have[names(BB_MIN)], sep = "=", collapse = ", "),
         ") — roster is invalid")
  }
  
  # 1. Take the minima: best BB_MIN[pos] players in each position.
  pool$in_xi <- pool$pos_rank <= BB_MIN[pool$position]
  
  # 2. Fill the remaining slots greedily by points, respecting BB_MAX.
  remaining <- BB_XI - sum(pool$in_xi)
  cand <- which(!pool$in_xi & pool$pos_rank <= BB_MAX[pool$position])
  cand <- cand[order(-pool$points[cand], -pool$tiebreak[cand],
                     pool$player_id[cand])]
  if (length(cand) < remaining) {
    stop("select_best_xi: cannot fill XI — only ", length(cand),
         " eligible candidates for ", remaining, " open slots")
  }
  pool$in_xi[cand[seq_len(remaining)]] <- TRUE
  
  pool$pos_rank <- NULL
  pool
}

# --- Roster resolution ----------------------------------------------------------

# Position for every rostered player, resolved in order of authority:
#   1. the players table (the contest's own positions — authoritative)
#   2. the roster's `slot` / `position` column (what was picked into)
#   3. any scored row for that player (last resort)
# Returns data.frame(player_id, position) with one row per player_id.
.bb_positions <- function(picks, scored = NULL, players = NULL) {
  want <- unique(as.character(picks$player_id))
  out  <- data.frame(player_id = want, position = NA_character_,
                     stringsAsFactors = FALSE)
  
  take <- function(ids, pos) {
    ids <- as.character(ids)
    pos <- toupper(trimws(as.character(pos)))
    pos[!pos %in% BB_POSITIONS] <- NA_character_
    keep <- !is.na(ids) & !is.na(pos)
    if (!any(keep)) return(invisible(NULL))
    ids <- ids[keep]
    pos <- pos[keep]
    idx  <- match(out$player_id, ids)
    fill <- is.na(out$position) & !is.na(idx)
    if (any(fill)) out$position[fill] <<- pos[idx[fill]]
    invisible(NULL)
  }
  
  if (!is.null(players) && nrow(players) > 0 &&
      all(c("player_id", "position") %in% names(players))) {
    take(players$player_id, players$position)
  }
  
  slot_col <- intersect(c("slot", "position"), names(picks))
  if (length(slot_col) > 0) take(picks$player_id, picks[[slot_col[1]]])
  
  if (!is.null(scored) && nrow(scored) > 0 && "position" %in% names(scored)) {
    take(scored$player_id, scored$position)
  }
  
  n_bad <- sum(is.na(out$position))
  if (n_bad > 0) {
    message("!!! bestball: ", n_bad, " rostered player_id(s) have no ",
            "resolvable position and are excluded from XI selection: ",
            paste(utils::head(out$player_id[is.na(out$position)], 8),
                  collapse = ", "))
  }
  out[!is.na(out$position), , drop = FALSE]
}

# FanTeam salary per player_id, for tie-breaking. NULL when the players
# table carries no fanteam_price column, which select_best_xi() treats as
# "no tie-break" and falls back to player_id.
.bb_prices <- function(players) {
  if (is.null(players) || !"fanteam_price" %in% names(players)) return(NULL)
  v <- suppressWarnings(as.numeric(players$fanteam_price))
  setNames(ifelse(is.na(v), 0, v), as.character(players$player_id))
}

# The player_ids an entry held for a given gameweek. `gw_period` is a
# named vector/list mapping gameweek (as character) -> period. Tolerant
# by design — see the header note.
.bb_entry_roster <- function(picks, eid, gw = NA, gw_period = NULL) {
  rows <- picks[as.character(picks$entry_id) == as.character(eid), ,
                drop = FALSE]
  if (nrow(rows) == 0) return(character(0))
  
  if (!is.null(gw_period) && !is.na(gw) && "period" %in% names(rows)) {
    want <- suppressWarnings(as.integer(gw_period[[as.character(gw)]]))
    pp   <- suppressWarnings(as.integer(rows$period))
    if (length(want) == 1L && !is.na(want) && any(!is.na(pp) & pp == want)) {
      rows <- rows[!is.na(pp) & pp == want, , drop = FALSE]
    }
  }
  unique(as.character(rows$player_id))
}

# --- Entry x gameweek totals ----------------------------------------------------

# scored:    output of score_player_match_stats() carrying a `gameweek`
#            column (added by assign_gameweeks() in R/gameweeks.R)
# picks:     roster_picks rows (entry_id, player_id, and ideally slot +
#            period)
# players:   the players table, for authoritative positions
# gw_period: named map gameweek -> period, from the gameweeks tab
#
# Returns one row per (entry_id, gameweek) with xi_points.
bestball_gameweek_points <- function(scored, picks, players = NULL,
                                     gw_period = NULL) {
  empty <- data.frame(entry_id = character(0), gameweek = numeric(0),
                      xi_points = numeric(0), stringsAsFactors = FALSE)
  
  if (is.null(scored) || nrow(scored) == 0) return(empty)
  if (is.null(picks)  || nrow(picks)  == 0) return(empty)
  
  message(">>> bestball_gameweek_points: ", nrow(scored), " scored rows, ",
          nrow(picks), " picks")
  
  if (!"gameweek" %in% names(scored)) {
    message("!!! bestball_gameweek_points: no `gameweek` column — run ",
            "assign_gameweeks() first. Returning no rows.")
    return(empty)
  }
  
  picks$player_id  <- as.character(picks$player_id)
  picks$entry_id   <- as.character(picks$entry_id)
  scored$player_id <- as.character(scored$player_id)
  
  # Rows outside every gameweek window score for nobody — dropped here
  # rather than folded into a phantom gameweek 0.
  gw_n  <- suppressWarnings(as.numeric(scored$gameweek))
  no_gw <- is.na(gw_n)
  if (any(no_gw)) {
    message("    bestball: dropping ", sum(no_gw),
            " scored row(s) with no gameweek (outside every window)")
    scored <- scored[!no_gw, , drop = FALSE]
    gw_n   <- gw_n[!no_gw]
  }
  if (nrow(scored) == 0) return(empty)
  scored$gameweek_n <- gw_n
  
  # Player points per gameweek — double gameweeks sum here, before XI
  # selection.
  per_pg  <- aggregate(points ~ player_id + gameweek_n, data = scored,
                       FUN = sum)
  pts_key <- setNames(per_pg$points,
                      paste0(per_pg$player_id, "|", per_pg$gameweek_n))
  
  gws <- sort(unique(per_pg$gameweek_n))
  if (length(gws) == 0) return(empty)
  
  roster_pos <- .bb_positions(picks, scored, players)
  pos_of     <- setNames(roster_pos$position, roster_pos$player_id)
  price_of   <- .bb_prices(players)
  if (is.null(price_of)) {
    message("    bestball: no fanteam_price on the players table — ties ",
            "fall back to player_id order")
  }
  
  entries_ids <- unique(picks$entry_id)
  out    <- vector("list", length(entries_ids) * length(gws))
  k      <- 0L
  failed <- character(0)
  
  for (eid in entries_ids) {
    for (gw in gws) {
      roster <- .bb_entry_roster(picks, eid, gw, gw_period)
      roster <- roster[roster %in% names(pos_of)]
      
      xi_pts <- 0
      if (length(roster) > 0) {
        pool <- data.frame(
          player_id = roster,
          position  = unname(pos_of[roster]),
          points    = unname(pts_key[paste0(roster, "|", gw)]),
          tiebreak  = if (is.null(price_of)) 0 else unname(price_of[roster]),
          stringsAsFactors = FALSE
        )
        pool$points <- ifelse(is.na(pool$points), 0, pool$points)
        
        xi <- tryCatch(select_best_xi(pool), error = function(e) {
          failed <<- c(failed, paste0(eid, "/GW", gw, ": ",
                                      conditionMessage(e)))
          NULL
        })
        if (!is.null(xi)) xi_pts <- sum(xi$points[xi$in_xi])
      }
      
      k <- k + 1L
      out[[k]] <- data.frame(entry_id = eid, gameweek = gw,
                             xi_points = xi_pts, stringsAsFactors = FALSE)
    }
  }
  
  if (length(failed) > 0) {
    message("!!! bestball: ", length(failed), " entry-gameweek(s) could not ",
            "field a legal XI and scored 0 — ",
            paste(utils::head(failed, 5), collapse = " | "))
  }
  
  res <- do.call(rbind, out[seq_len(k)])
  message("    bestball: ", length(entries_ids), " entries x ", length(gws),
          " gameweek(s) = ", nrow(res), " rows, total XI points ",
          round(sum(res$xi_points), 1))
  res
}

# --- Leaderboard ----------------------------------------------------------------

# The contest leaderboard: entries ranked by total bestball points
# across all gameweeks present in `scored`.
bestball_leaderboard <- function(scored, picks, entries, players = NULL,
                                 gw_period = NULL) {
  message(">>> bestball_leaderboard: ", nrow(entries), " entries")
  
  entries$entry_id <- as.character(entries$entry_id)
  
  gw <- bestball_gameweek_points(scored, picks, players, gw_period)
  if (nrow(gw) == 0) {
    entries$total_points <- 0
    return(entries[order(entries$entry_name), ])
  }
  
  totals <- aggregate(xi_points ~ entry_id, data = gw, FUN = sum)
  names(totals)[names(totals) == "xi_points"] <- "total_points"
  
  out <- merge(entries, totals, by = "entry_id", all.x = TRUE)
  out$total_points <- ifelse(is.na(out$total_points), 0, out$total_points)
  out[order(-out$total_points, out$entry_name), ]
}

# --- Single squad, single gameweek ----------------------------------------------

# One entry's 15 (or whatever they hold for that gameweek's period) with
# that gameweek's points and an `in_xi` flag — what the leaderboard's
# squad panel renders when a gameweek pill is selected.
#
# `gw = NA` means "whole contest to date": points are each player's total
# across every gameweek, and in_xi is NA, because there is no single XI
# spanning the season — that is the opening-squad view.
#
# Returns data.frame(player_id, position, points, in_xi), or NULL if the
# entry holds nothing.
bestball_entry_xi <- function(scored, picks, entry_id, gw = NA,
                              players = NULL, gw_period = NULL) {
  if (is.null(picks) || nrow(picks) == 0) return(NULL)
  
  picks$player_id <- as.character(picks$player_id)
  picks$entry_id  <- as.character(picks$entry_id)
  entry_id        <- as.character(entry_id)
  
  roster <- .bb_entry_roster(picks, entry_id, gw, gw_period)
  if (length(roster) == 0) return(NULL)
  
  mine       <- picks[picks$entry_id == entry_id, , drop = FALSE]
  roster_pos <- .bb_positions(mine, scored, players)
  pos_of     <- setNames(roster_pos$position, roster_pos$player_id)
  roster     <- roster[roster %in% names(pos_of)]
  if (length(roster) == 0) return(NULL)
  
  pts <- setNames(rep(0, length(roster)), roster)
  if (!is.null(scored) && nrow(scored) > 0) {
    s <- scored
    s$player_id <- as.character(s$player_id)
    if (!is.na(gw)) {
      if (!"gameweek" %in% names(s)) return(NULL)
      keep <- !is.na(s$gameweek) &
        suppressWarnings(as.numeric(s$gameweek)) == gw
      s <- s[keep, , drop = FALSE]
    }
    s <- s[s$player_id %in% roster, , drop = FALSE]
    if (nrow(s) > 0) {
      agg <- aggregate(points ~ player_id, data = s, FUN = sum)
      pts[agg$player_id] <- agg$points
    }
  }
  
  price_of <- .bb_prices(players)
  pool <- data.frame(
    player_id = roster,
    position  = unname(pos_of[roster]),
    points    = unname(pts[roster]),
    tiebreak  = if (is.null(price_of)) 0 else unname(price_of[roster]),
    stringsAsFactors = FALSE
  )
  
  # No XI for the season-to-date view: the optimal XI is a per-gameweek
  # concept, and marking one across the whole season would be a lie.
  if (is.na(gw)) {
    pool$in_xi <- NA
    return(pool)
  }
  
  xi <- tryCatch(select_best_xi(pool), error = function(e) {
    message("!!! bestball_entry_xi: entry ", entry_id, " GW", gw, ": ",
            conditionMessage(e))
    NULL
  })
  if (is.null(xi)) {
    pool$in_xi <- NA
    return(pool)
  }
  xi
}

# --- Standout performers ---------------------------------------------------

BB_HOT_N <- 5L

# The player_ids inside the top N scorers AT THEIR POSITION across the whole
# league for one gameweek — the flame on the leaderboard's score discs.
#
# Two deliberate choices. Ties at the cut are all included (rank with
# ties.method = "min"), because excluding one of two identical scores by an
# arbitrary tiebreak would be indefensible on a board people are checking
# against each other. And a zero or negative score never qualifies, however
# few players have appeared: early in a gameweek only a couple of clubs have
# played, and a flame on a goalkeeper sitting at 0.0 because he is one of
# only three keepers with a score would read as broken.
#
# Double gameweeks are summed per player before ranking, matching how the
# points are earned.
bestball_hot_players <- function(scored, gw, n = BB_HOT_N) {
  if (is.null(scored) || nrow(scored) == 0) return(character(0))
  need <- c("gameweek", "position", "points", "player_id")
  if (!all(need %in% names(scored))) return(character(0))
  
  g <- suppressWarnings(as.numeric(scored$gameweek))
  s <- scored[!is.na(g) & g == gw, , drop = FALSE]
  if (nrow(s) == 0) return(character(0))
  
  s$player_id <- as.character(s$player_id)
  agg <- aggregate(points ~ player_id + position, data = s, FUN = sum)
  agg <- agg[!is.na(agg$points) & agg$points > 0, , drop = FALSE]
  if (nrow(agg) == 0) return(character(0))
  
  hot <- lapply(split(agg, agg$position), function(d) {
    d$player_id[rank(-d$points, ties.method = "min") <= n]
  })
  unique(as.character(unlist(hot, use.names = FALSE)))
}

# Local numeric coercion (mirrors scoring.R's .num0; duplicated so this
# file stays standalone if sourced in isolation for console checks).
.num0_bb <- function(x) {
  out <- suppressWarnings(as.numeric(x))
  ifelse(is.na(out), 0, out)
}