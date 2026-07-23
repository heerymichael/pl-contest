# App-level data layer.
#
# Players and teams are static reference data — squads are final — so
# they're loaded once at startup from local RDS snapshots
# (data/players_snapshot.rds, data/teams_snapshot.rds), created by
# snapshot_reference_data.R. The Sheet remains the source of truth:
# if squads ever change again, re-run ingest_squads.R then the
# snapshot script, and restart the app.
#
# get_players_cached() / get_teams_cached() keep their names so no
# call sites change. Contest config stays on the 5-minute TTL
# memoised cache — lock_at needs to stay live.


message(">>> cache.R sourced by:\n",
        paste(utils::capture.output(traceback()), collapse = "\n"),
        "\n---stack---\n",
        paste(sapply(sys.calls(), function(c) deparse(c)[1]), collapse = "\n"))


library(memoise)
library(cachem)

message(">>> SOURCED R/cache.R at ", format(Sys.time()))

# --- Static reference data (loaded once at startup) --------------------

.players_all <- readRDS("data/players_snapshot.rds")
message("    [cache] loaded players snapshot — ", nrow(.players_all),
        " rows (", sum(.players_all$is_active), " active)")

.teams_all <- readRDS("data/teams_snapshot.rds")
message("    [cache] loaded teams snapshot — ", nrow(.teams_all), " rows")

.players_active <- dplyr::filter(.players_all, is_active == TRUE)

get_players_cached <- function() {
  .players_active
}

get_teams_cached <- function() {
  .teams_all
}

# Full player table including inactive rows — for pick joins, so a pick
# pointing at a deactivated player still resolves to a name. Used by
# get_picks_for_entry() in R/sheets.R.
get_players_all_local <- function() {
  .players_all
}

# --- Contest data snapshots (entries + roster_picks) --------------------
# Created by snapshot_contest_data.R; immutable after lock. If the
# snapshot files are missing, the accessors return NULL and the
# leaderboard falls back to live Sheets reads (slow but correct) — so
# a bundle without snapshots degrades gracefully instead of crashing.

.load_snapshot <- function(path, label) {
  if (!file.exists(path)) {
    message("    [cache] NO ", label, " snapshot at ", path,
            " — leaderboard will fall back to live Sheets reads.",
            " Run snapshot_contest_data.R to create it.")
    return(NULL)
  }
  out <- readRDS(path)
  taken <- attr(out, "snapshot_at")
  taken_txt <- if (is.null(taken)) "unknown" else format(taken)
  message("    [cache] loaded ", label, " snapshot — ", nrow(out),
          " rows (taken ", taken_txt, ")")
  out
}

.entries_snapshot <- .load_snapshot("data/entries_snapshot.rds", "entries")
.picks_snapshot   <- .load_snapshot("data/roster_picks_snapshot.rds",
                                    "roster_picks")

# All entries from the local snapshot, or NULL if no snapshot exists.
get_entries_local <- function() {
  .entries_snapshot
}

# All roster picks from the local snapshot, or NULL if no snapshot.
get_all_picks_local <- function() {
  .picks_snapshot
}

# Picks for one entry, resolved entirely from local snapshots (picks +
# players) — zero Sheets reads. Returns NULL if no picks snapshot
# exists, so callers can fall back to get_picks_for_entry().
# Mirrors the output columns of get_picks_for_entry() in R/sheets.R.
get_picks_for_entry_local <- function(entry_id) {
  if (is.null(.picks_snapshot)) return(NULL)
  entry_picks <- dplyr::filter(.picks_snapshot, entry_id == !!entry_id)
  if (nrow(entry_picks) == 0) {
    return(dplyr::tibble(player_id = integer(0), name = character(0),
                         team = character(0), position = character(0)))
  }
  players <- get_players_all_local()
  entry_picks |>
    dplyr::select(player_id) |>
    dplyr::left_join(players |>
                       dplyr::select(player_id, name, team, position),
                     by = "player_id")
}

# --- Live config (5-min TTL memoised) ----------------------------------

.cache_ttl_seconds <- 5 * 60
.cache_store       <- cache_mem(max_age = .cache_ttl_seconds)

get_config_cached <- local({
  underlying <- memoise(
    function() {
      message("    [cache] MISS get_contest_config — fetching from Sheets")
      get_contest_config()
    },
    cache = .cache_store
  )
  function() {
    message(">>> get_config_cached called")
    underlying()
  }
})


# --- Match stats (5-min TTL memoised) -----------------------------------
# player_match_stats changes only when ingest_match_stats.R runs, so the
# leaderboard reads it through the same 5-minute store as contest config:
# fast between misses, and an ingest run reaches the public board within
# five minutes with no redeploy.

get_match_stats_cached <- local({
  underlying <- memoise(
    function() {
      message("    [cache] MISS player_match_stats — fetching from Sheets")
      get_player_match_stats()
    },
    cache = .cache_store
  )
  function() {
    underlying()
  }
})