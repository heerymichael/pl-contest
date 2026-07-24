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
# Pure functions, same contract as R/scoring.R: no side effects, no
# Sheets reads. Consumes score_player_match_stats() output.
#
# Main entry points:
#   select_best_xi(pool)                        -> one gameweek, one entry
#   bestball_gameweek_points(scored, picks)     -> entry x gameweek totals
#   bestball_leaderboard(scored, picks, entries)-> the contest leaderboard
# ------------------------------------------------------------------------------

# Formation bounds. Selection = exactly BB_MIN per position, then the 4
# remaining slots greedily filled by points subject to BB_MAX. With the
# minima fixed, greedy fill respecting per-position caps is optimal
# (independent per-position caps — no interaction between positions).
BB_MIN <- c(GK = 1, DEF = 3, MID = 2, FWD = 1)
BB_MAX <- c(GK = 1, DEF = 5, MID = 5, FWD = 3)
BB_XI  <- 11L

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

  # Order within position by points desc (ties: player_id for determinism)
  pool <- pool[order(pool$position, -pool$points, pool$player_id), ]
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
  cand <- cand[order(-pool$points[cand])]
  if (length(cand) < remaining) {
    stop("select_best_xi: cannot fill XI — only ", length(cand),
         " eligible candidates for ", remaining, " open slots")
  }
  pool$in_xi[cand[seq_len(remaining)]] <- TRUE

  pool$pos_rank <- NULL
  pool
}

# --- Entry x gameweek totals ----------------------------------------------------

# scored: output of score_player_match_stats() — must carry a `gameweek`
#         column (numeric or coercible)
# picks:  roster_picks rows (entry_id, player_id)
#
# Returns one row per (entry_id, gameweek) with xi_points.
bestball_gameweek_points <- function(scored, picks) {
  message(">>> bestball_gameweek_points: ", nrow(scored), " scored rows, ",
          nrow(picks), " picks")

  picks$player_id <- as.character(picks$player_id)
  picks$entry_id  <- as.character(picks$entry_id)
  scored$player_id <- as.character(scored$player_id)

  gws <- sort(unique(.num0_bb(scored$gameweek)))
  if (length(gws) == 0) {
    return(data.frame(entry_id = character(0), gameweek = numeric(0),
                      xi_points = numeric(0)))
  }

  # Player points per gameweek (double GWs sum here), plus position.
  scored$gameweek_n <- .num0_bb(scored$gameweek)
  per_pg <- aggregate(points ~ player_id + gameweek_n + position,
                      data = scored, FUN = sum)

  # Position lookup for rostered players who haven't appeared yet in a
  # given gameweek — take each player's position from any scored row;
  # players with no scored rows at all contribute 0 and are excluded
  # from selection only if their position is unknowable, in which case
  # the minima are still guaranteed by the other rostered players until
  # the roster's players appear. (In practice the players tab position
  # is joined in at scoring time, so every rostered player who has ever
  # appeared carries a position.)
  pos_lookup <- unique(per_pg[, c("player_id", "position")])

  entries_ids <- unique(picks$entry_id)
  out <- vector("list", length(entries_ids) * length(gws))
  k <- 0L

  for (eid in entries_ids) {
    roster <- picks$player_id[picks$entry_id == eid]
    roster_pos <- pos_lookup[pos_lookup$player_id %in% roster, ]

    for (gw in gws) {
      gw_pts <- per_pg[per_pg$gameweek_n == gw &
                         per_pg$player_id %in% roster, ]
      pool <- merge(roster_pos, gw_pts[, c("player_id", "points")],
                    by = "player_id", all.x = TRUE)
      pool$points <- ifelse(is.na(pool$points), 0, pool$points)

      xi_pts <- if (nrow(pool) == 0) 0 else {
        xi <- tryCatch(select_best_xi(pool), error = function(e) {
          message("!!! bestball: entry ", eid, " GW", gw, ": ",
                  conditionMessage(e), " — scoring 0 for this gameweek")
          NULL
        })
        if (is.null(xi)) 0 else sum(xi$points[xi$in_xi])
      }

      k <- k + 1L
      out[[k]] <- data.frame(entry_id = eid, gameweek = gw,
                             xi_points = xi_pts)
    }
  }

  do.call(rbind, out[seq_len(k)])
}

# --- Leaderboard ----------------------------------------------------------------

# The contest leaderboard: entries ranked by total bestball points
# across all gameweeks present in `scored`.
bestball_leaderboard <- function(scored, picks, entries) {
  message(">>> bestball_leaderboard: ", nrow(entries), " entries")

  entries$entry_id <- as.character(entries$entry_id)

  gw <- bestball_gameweek_points(scored, picks)
  totals <- aggregate(xi_points ~ entry_id, data = gw, FUN = sum)
  names(totals)[names(totals) == "xi_points"] <- "total_points"

  out <- merge(entries, totals, by = "entry_id", all.x = TRUE)
  out$total_points <- ifelse(is.na(out$total_points), 0, out$total_points)
  out[order(-out$total_points, out$entry_name), ]
}

# Local numeric coercion (mirrors scoring.R's .num0; duplicated so this
# file stays standalone if sourced in isolation for console checks).
.num0_bb <- function(x) {
  out <- suppressWarnings(as.numeric(x))
  ifelse(is.na(out), 0, out)
}
