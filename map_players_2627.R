# map_players_2627.R --------------------------------------------------------
# Close api_player_id gaps on the players tab using players who have actually
# appeared in 26/27, as recorded on the player_match_stats tab.
#
# EDIT DRY_RUN IN THIS FILE, not at the console.
#
# Run after:  source("R/sheets.R"); gs_auth()
#             and after at least one ingest_match_stats.R live write
#
# --- WHY THIS EXISTS --------------------------------------------------------
#
# map_players_2526.R matched the pool against pl2526_scores.rds, so anyone who
# did not appear in the Premier League last season was structurally
# unmatchable: summer arrivals, promoted-club squads, players back from
# abroad. Those ids can only come from 26/27 data.
#
# The source here is the ingest's own output. Every appearance writes a row
# carrying api_player_id, player_name and the club short code, so the moment a
# player features they become mappable — and a player who has never featured
# has no points to lose by staying unmapped. Run this after each ingest and
# the gap closes itself.
#
# --- MATCHING RULES ---------------------------------------------------------
#
# Auto-match requires normalised full-name equality WITHIN THE SAME CLUB.
# Club agreement is what makes this safe: a bare league-wide name match is how
# the Gabriel problem happened. Anything short of that is REPORTED as an
# override candidate, never applied.
#
# Refused outright, in every case reported rather than guessed:
#   - a pool player matching more than one API player at their club
#   - an API player matching more than one pool player at their club
#   - an API id already assigned to a different pool player
#   - any pool player whose api_player_id is already non-blank (never
#     overwritten; a disagreement is reported as a conflict)
#
# Near-misses (same club, same surname, different full name) are printed as
# override candidates for data/api_id_overrides_2627.csv. That file always
# wins and is the only route to a match this script won't make itself.
# ---------------------------------------------------------------------------

DRY_RUN <- TRUE

OVERRIDES_CSV <- "data/api_id_overrides_2627.csv"   # optional
REVIEW_CSV    <- "/tmp/map_players_2627_review.csv"

suppressPackageStartupMessages({
  library(googlesheets4)
  library(dplyr)
})

message(">>> SOURCED map_players_2627.R at ", format(Sys.time()),
        if (DRY_RUN) "  [DRY RUN]" else "  [LIVE WRITE]")

SHEET <- if (exists("SHEET_ID")) get("SHEET_ID") else
  "1oFdL_U-U4G-3YGXd13Pcb6s4-U01reZwEeVQ-ceUdI8"

# Same normaliser as map_players_2526.R. chartr first: iconv ASCII//TRANSLIT
# drops stroked/slashed letters (Đ, Ł, Ø) on some platforms instead of
# converting them.
norm_name <- function(x) {
  x <- chartr("\u0110\u0111\u0141\u0142\u00D8\u00F8", "DdLlOo", x)
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  x[is.na(x)] <- ""
  gsub("[^a-z]", "", tolower(x))
}

surname_of <- function(x) {
  parts <- strsplit(trimws(as.character(x)), "\\s+")
  vapply(parts, function(p) if (length(p) == 0) "" else p[length(p)],
         character(1))
}

is_blank <- function(x) {
  x <- trimws(as.character(x))
  is.na(x) | !nzchar(x) | toupper(x) %in% c("NA", "NULL")
}

# --- read inputs ------------------------------------------------------------

players <- read_sheet(SHEET, sheet = "players", col_types = "c")
stopifnot("api_player_id" %in% names(players),
          "player_id" %in% names(players),
          "team" %in% names(players))
players <- players |> mutate(across(everything(), as.character))

stats <- tryCatch(read_sheet(SHEET, sheet = "player_match_stats",
                             col_types = "c"),
                  error = function(e) NULL)
if (is.null(stats) || nrow(stats) == 0) {
  stop("player_match_stats is empty or missing — run ingest_match_stats.R ",
       "with DRY_RUN <- FALSE first.", call. = FALSE)
}

message("    [map27] players ", nrow(players), " (",
        sum(!is_blank(players$api_player_id)), " already mapped) | ",
        "stats ", nrow(stats), " rows across ",
        length(unique(stats$match_id)), " match(es)")

# --- the API-side directory -------------------------------------------------
# One row per (api_player_id, club). A player who moves club mid-season would
# legitimately appear twice; both are candidates and club decides.

seen <- stats |>
  filter(!is_blank(api_player_id)) |>
  distinct(api_player_id, player_name, team) |>
  mutate(nn = norm_name(player_name),
         sn = norm_name(surname_of(player_name)))

message("    [map27] ", length(unique(seen$api_player_id)),
        " distinct API players seen so far")

taken <- players |> filter(!is_blank(api_player_id)) |> pull(api_player_id)
free  <- seen |> filter(!api_player_id %in% taken)

message("    [map27] ", nrow(free), " API player-club row(s) not yet ",
        "assigned to any pool player")

# --- the pool side ----------------------------------------------------------

gap <- players |>
  filter(is_blank(api_player_id)) |>
  mutate(nn = norm_name(name), sn = norm_name(surname_of(name)))

message("    [map27] ", nrow(gap), " pool player(s) awaiting an api_player_id")

# --- auto-match: full name within club --------------------------------------

cand <- gap |>
  select(player_id, name, team, position, nn, sn) |>
  inner_join(free |> select(api_player_id, api_name = player_name,
                            team, nn),
             by = c("nn", "team"))

# Ambiguity in either direction disqualifies the match entirely.
amb_pool <- cand |> count(player_id) |> filter(n > 1) |> pull(player_id)
amb_api  <- cand |> count(api_player_id) |> filter(n > 1) |> pull(api_player_id)

if (length(amb_pool) > 0 || length(amb_api) > 0) {
  message("!!  [map27] ", length(amb_pool), " pool player(s) and ",
          length(amb_api), " API player(s) excluded as ambiguous:")
  print(as.data.frame(
    cand |> filter(player_id %in% amb_pool | api_player_id %in% amb_api) |>
      select(player_id, name, team, api_player_id, api_name)),
    row.names = FALSE)
}

auto <- cand |>
  filter(!player_id %in% amb_pool, !api_player_id %in% amb_api) |>
  transmute(player_id, name, team, position,
            final = api_player_id, matched_as = api_name, source = "name")

# --- overrides (always win) -------------------------------------------------

ov <- if (file.exists(OVERRIDES_CSV)) {
  read.csv(OVERRIDES_CSV, colClasses = "character", stringsAsFactors = FALSE)
} else {
  message("    [map27] no overrides file at ", OVERRIDES_CSV,
          " — name matching only")
  data.frame(player_id = character(0), api_player_id = character(0),
             stringsAsFactors = FALSE)
}

if (nrow(ov) > 0) {
  stopifnot(all(c("player_id", "api_player_id") %in% names(ov)))
  bad_pid <- setdiff(ov$player_id, players$player_id)
  if (length(bad_pid) > 0) {
    stop("Override player_id(s) not on the players tab: ",
         paste(bad_pid, collapse = ", "), call. = FALSE)
  }
  bad_api <- setdiff(ov$api_player_id, seen$api_player_id)
  if (length(bad_api) > 0) {
    stop("Override api_player_id(s) never seen in player_match_stats: ",
         paste(bad_api, collapse = ", "),
         ". Only ids that have actually appeared can be assigned.",
         call. = FALSE)
  }
  # An override that has already been applied — its api id sits on exactly
  # the pool player it targets — is a no-op, not an error. The overrides
  # file is a cumulative record of confirmed identities and must survive
  # its own success: without this, every run after the first live one dies
  # on the rows it applied. Only an id assigned to a DIFFERENT pool player
  # is a genuine clash.
  cur_api <- setNames(as.character(players$api_player_id),
                      as.character(players$player_id))
  already <- vapply(seq_len(nrow(ov)), function(i) {
    identical(unname(cur_api[ov$player_id[i]]), ov$api_player_id[i])
  }, logical(1))
  if (any(already)) {
    message("    [map27] ", sum(already), " override(s) already applied in ",
            "a previous run — skipped: ",
            paste(ov$api_player_id[already], collapse = ", "))
    ov <- ov[!already, , drop = FALSE]
  }
  
  clash <- intersect(ov$api_player_id, taken)
  if (length(clash) > 0) {
    stop("Override api_player_id(s) already assigned to a DIFFERENT pool ",
         "player: ", paste(clash, collapse = ", "), call. = FALSE)
  }
  
  ov_rows <- ov |>
    left_join(players |> select(player_id, name, team, position),
              by = "player_id") |>
    left_join(seen |> distinct(api_player_id, .keep_all = TRUE) |>
                select(api_player_id, api_name = player_name),
              by = "api_player_id") |>
    transmute(player_id, name, team, position,
              final = api_player_id, matched_as = api_name,
              source = "override")
  
  # Overrides replace any auto-match for the same player.
  auto <- auto |> filter(!player_id %in% ov_rows$player_id)
  auto <- bind_rows(ov_rows, auto)
}

# Final safety: no api id may be assigned twice within this proposal.
dup_final <- auto |> count(final) |> filter(n > 1)
if (nrow(dup_final) > 0) {
  print(as.data.frame(auto |> filter(final %in% dup_final$final)),
        row.names = FALSE)
  stop("Refusing to write: api id(s) proposed for more than one pool ",
       "player.", call. = FALSE)
}

# --- near-misses: override candidates ---------------------------------------
# Same club, same surname, full names differ. Never auto-applied.

near <- gap |>
  filter(!player_id %in% auto$player_id) |>
  select(player_id, name, team, position, sn) |>
  inner_join(free |> filter(!api_player_id %in% auto$final) |>
               select(api_player_id, api_name = player_name, team, sn),
             by = c("sn", "team"))

message("")
message(">>> [map27] ============== RESULT ==============")
message(">>> [map27] auto-matched by name+club : ",
        sum(auto$source == "name"))
message(">>> [map27] applied from overrides    : ",
        sum(auto$source == "override"))
message(">>> [map27] still unmapped            : ",
        nrow(gap) - nrow(auto))

if (nrow(auto) > 0) {
  message("")
  message(">>> [map27] proposed mappings:")
  print(as.data.frame(auto |> arrange(team, name)), row.names = FALSE)
}

if (nrow(near) > 0) {
  message("")
  message(">>> [map27] SURNAME near-misses \u2014 same club, same surname, ",
          "full names differ. NOT applied. Confirm each by eye and add to ",
          OVERRIDES_CSV, " (columns: player_id, api_player_id):")
  print(as.data.frame(near |> select(player_id, name, team, position,
                                     api_player_id, api_name)),
        row.names = FALSE)
}

# Owned players still unmapped are the ones that actually cost points.
picks <- tryCatch(read_sheet(SHEET, sheet = "roster_picks", col_types = "c"),
                  error = function(e) NULL)
if (!is.null(picks) && nrow(picks) > 0) {
  owned <- unique(as.character(picks$player_id))
  still <- gap |> filter(!player_id %in% auto$player_id,
                         player_id %in% owned)
  message("")
  if (nrow(still) == 0) {
    message(">>> [map27] every OWNED player is now mapped.")
  } else {
    message(">>> [map27] ", nrow(still), " OWNED player(s) still unmapped ",
            "(they have not appeared yet, so nothing is lost until they do):")
    print(as.data.frame(still |> select(player_id, name, team, position)),
          row.names = FALSE)
  }
}

# --- write ------------------------------------------------------------------

full <- players |>
  left_join(auto |> select(player_id, final), by = "player_id") |>
  mutate(new_api = ifelse(is_blank(api_player_id),
                          ifelse(is.na(final), "", final),
                          api_player_id))

write.csv(
  full |> select(player_id, name, team, position, api_player_id, final,
                 new_api),
  REVIEW_CSV, row.names = FALSE, na = ""
)
message("")
message("    [map27] full table -> ", REVIEW_CSV)

n_before <- sum(!is_blank(players$api_player_id))
n_after  <- sum(!is_blank(full$new_api))

if (DRY_RUN) {
  message(">>> [map27] DRY RUN — nothing written. Coverage would go ",
          n_before, " -> ", n_after, " of ", nrow(players), ".")
  message(">>> [map27] Review, then edit DRY_RUN <- FALSE in THIS FILE and ",
          "re-source.")
} else {
  col_idx <- which(names(players) == "api_player_id")
  stopifnot(length(col_idx) == 1, col_idx <= 26)
  col_letter <- LETTERS[col_idx]
  
  out <- tibble::tibble(api_player_id = full$new_api)
  message(">>> [map27] writing api_player_id (", n_after,
          " ids) to players tab column ", col_letter)
  range_write(SHEET, data = out, sheet = "players",
              range = paste0(col_letter, "1"), col_names = TRUE)
  
  # --- hard verification ----------------------------------------------------
  chk <- read_sheet(SHEET, sheet = "players", col_types = "c")
  n_chk    <- sum(!is_blank(chk$api_player_id))
  ok_count <- n_chk == n_after
  ok_rows  <- nrow(chk) == nrow(players)
  # Every id we just added must be readable back on the right row.
  ok_spot <- TRUE
  if (nrow(auto) > 0) {
    for (i in seq_len(nrow(auto))) {
      got <- chk$api_player_id[chk$player_id == auto$player_id[i]]
      if (length(got) != 1 || !identical(trimws(got), auto$final[i])) {
        message("    [verify] FAIL ", auto$name[i], " (", auto$player_id[i],
                ") -> ", if (length(got)) got else "<missing>",
                ", expected ", auto$final[i])
        ok_spot <- FALSE
      }
    }
  }
  # No id may appear twice on the tab.
  dup_tab <- chk |> filter(!is_blank(api_player_id)) |>
    count(api_player_id) |> filter(n > 1)
  ok_uniq <- nrow(dup_tab) == 0
  if (!ok_uniq) {
    message("    [verify] FAIL duplicate api ids on the tab: ",
            paste(dup_tab$api_player_id, collapse = ", "))
  }
  
  message("    [verify] populated ids: ", n_chk, " (expected ", n_after, ") ",
          if (ok_count) "OK" else "FAIL")
  message("    [verify] row count unchanged: ", nrow(chk), " ",
          if (ok_rows) "OK" else "FAIL")
  message("    [verify] every new id reads back: ",
          if (ok_spot) "OK" else "FAIL")
  message("    [verify] api ids unique: ", if (ok_uniq) "OK" else "FAIL")
  
  if (!(ok_count && ok_rows && ok_spot && ok_uniq)) {
    stop("VERIFICATION FAILED — inspect the players tab before proceeding.",
         call. = FALSE)
  }
  message(">>> [map27] complete and verified. Next:")
  message(">>>   source(\"snapshot_pl_reference.R\")   # refresh the app's ",
          "players snapshot")
  message(">>>   then reset DRY_RUN <- TRUE in this file")
}
