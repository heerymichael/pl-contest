# Stats Update — Runbook

Fast reference for running and troubleshooting a match-stats ingestion. The
goal of this sheet is that any flag in the report maps to a known fix without
re-deriving anything.

---

## The routine

From the **project root** (relative paths depend on it):

```r
source("run_stats_update.R")
```

Then copy everything between the `vvvvv` / `^^^^^` markers in the report and
paste it back. That one block carries versions, whether the fetch was live, the
fixtures verdict, and the **NEEDS ATTENTION** list.

There is **no separate scoring step** — `R/scoring.R` runs at read time and the
leaderboard reflects the new rows within ~5 minutes. No redeploy.

**Clean run** = `NEEDS ATTENTION: none` and either every finished match already
ingested, or `writing player_match_stats: … across N match(es)`.

---

## Quick triage — signal → meaning → action

Read the report's **NEEDS ATTENTION** block first; most lines below appear there.

| Signal | Means | Action |
|---|---|---|
| `*** OWNED PLAYER UNMAPPED — match SKIPPED ***` | An **owned** player on a participating team has no `api_player_id`. Whole match withheld so nobody is silently under-scored. The gate is **team-level** — it fires whether or not that player actually played this match. | **Fix A** if the player has (or will have) an API record. **Fix C** if they've withdrawn / left the tournament. The console names the player + `player_id` on the indented line(s) under the flag. |
| `*** MISMATCH — payload goals=X vs fixture total=Y ***` | `Y−X` goals in the scoreline aren't credited to any player — almost always own goal(s). Match withheld. | **Fix B** below. Gap = number of own goals. |
| `dropped N unmapped non-owned player rows (of M who played)` | Unmapped players that **nobody owns** were excluded from the match. | **None.** Harmless — they score for no one. Map later only if you want them in the ownership table. |
| `SKIP — fixture score missing` / `SKIP — player-stats unavailable or empty` | API hasn't published final data for that match yet (lag). | **None.** Auto-retried on the next run. |
| `no new mappings — Sheet untouched` (map step) | Nothing new to auto-map this run. | **None, if expected.** If you *just* hand-mapped or ran `map_owned_overrides.R` and expected a change, column H wasn't saved — re-check. |
| `nothing new to write` (ingest step) | No new finished matches, **or** every candidate was skipped. | Check the attention lines — if a match skipped, fix it; otherwise nothing to do. |
| `Live fetch: NO` (report header) | Fixtures may be stale — wrong working directory, or cache not cleared. | `cd` to project root, re-run. |
| `[teams] WARNING` / fewer than 48 teams mapped | A team didn't map (rare). | Hand-map the team in the `teams` tab, re-run. |
| `HTTP 4xx/5xx` | API or network error. | Re-run. If persistent, check API key / TheStatsAPI status. |

---

## Fix A — Owned player unmapped (match skipped)

A nickname or bare first name the auto-matcher won't guess (e.g. Bono→Bounou,
"Gabriel"→Gabriel Magalhães). Steps:

> If the named player has **withdrawn from the tournament** and will never have
> an API record, skip to **Fix C** — there is nothing to map to, and the steps
> below will spin in place.

1. The gate names the owned player and `player_id`, e.g. `Yassine Bounou (MAR, player_id 767)`.

2. **Confirm the API id from the cached payload** — match by team + minutes so
   you never guess between two similar names (the two-Danilo trap). The match's
   player-stats payload is cached at
   `data/api_cache/_matches_<match_id>_player_stats.json`:

   ```r
   mid <- "mt_153637518"   # the skipped match
   library(jsonlite); `%||%` <- function(a,b) if (is.null(a)) b else a
   ps <- fromJSON(sprintf("data/api_cache/_matches_%s_player_stats.json", mid), simplifyVector = FALSE)
   played <- Filter(function(p) (p$minutes_played %||% 0) > 0, ps$data)
   out <- do.call(rbind, lapply(played, function(p) data.frame(
     name = p$player_name %||% "?", team = p$team_id %||% "?",
     mins = p$minutes_played %||% 0, goals = p$shooting$goals %||% 0,
     api_id = p$player_id %||% "?", stringsAsFactors = FALSE)))
   out[order(out$team, -out$goals), ]
   ```

   Find the player by name; the `api_id` column is the value to map. Confirm the
   position/minutes line up (Danilo the DEF played 45', Danilo Oliveira the MID
   played 10' — different people, different ids).

3. **Add the mapping** to the `OVERRIDES` block in `map_owned_overrides.R`
   (`player_id`, `api_player_id`, `label`).

4. **Dry-run, then write:** `source("map_owned_overrides.R")` with `DRY_RUN <- TRUE`,
   eyeball the `<empty> -> pl_…` line, set `DRY_RUN <- FALSE`, source again →
   expect `write complete`.

5. `source("run_stats_update.R")` → the match clears the gate and writes.

---

## Fix B — Own-goal mismatch (match skipped)

1. The gate prints `payload goals=X vs fixture total=Y`. The gap (`Y−X`) is the
   number of own goals — usually 1.

2. **Identify the own goal** — scorer, team, minute — from the match report.

3. **Edit `ingest_match_stats.R` IN THE FILE** (see gotcha — *not* the console).
   Add an entry to `OWN_GOAL_ALLOWANCE`, annotated:

   ```r
   OWN_GOAL_ALLOWANCE <- c(
     "mt_517473281" = 1,   # USA 4-1 PAR: Bobadilla (PAR) o.g., 7'
     "mt_641919660" = 1    # QAT 1-1 SUI: Muheim (SUI) o.g., 90+4'
   )
   ```

   **Save the file.**

4. `source("run_stats_update.R")` → `X + allowance = Y` now matches, **no
   mismatch line** for that match, it writes.

5. **Apply the −5:** set `own_goals = 1` on the OG scorer's row for that match in
   the `player_match_stats` tab. Manual columns are preserved verbatim on every
   re-ingest, so it sticks. (Harmless if the scorer isn't owned.)

---

## Fix C — Owned player who has withdrawn / will never appear in the API

Same gate as Fix A (`*** OWNED PLAYER UNMAPPED — match SKIPPED ***`), but the
named owned player is **confirmed out of the tournament** — withdrew, injured
out, or cut from the final squad after entries had drafted them. No API record
will ever exist for them, so they can never auto-map and **Fix A has nothing to
map *to*.** Left alone, the team-level gate withholds *every one of their team's
matches for the rest of the tournament*.

**First, rule out the look-alike case.** A player who simply *hasn't played yet*
(named to the squad, omitted from a matchday group) is NOT this case — they
auto-map the first time they appear in any match payload, and the blocked match
ingests on the next run after that. When in doubt, prefer to wait one matchday.
Only reach for the sentinel below once the player is **confirmed done** for the
tournament.

**Fix — `WITHDRAWN` sentinel in column H:**

1. Confirm the player is genuinely out (withdrawn / cut), not just rested for one
   game.
2. On the `players` tab, set their `api_player_id` cell (column H, on their
   `player_id` row) to `WITHDRAWN`.
3. `source("run_stats_update.R")` → the gate only checks that column H is
   non-empty, so it clears, and the match writes.
4. Log them in the living-record table below.

**Why the sentinel is inert everywhere except the gate:**

- The gate test is `is.na | !nzchar` — any non-empty value reads as "accounted
  for", so the team's matches stop being withheld.
- `WITHDRAWN` never equals a real payload id, so the player joins no stats row
  and scores zero — correct, since they didn't play.
- `map_api_players.R` never overwrites a non-empty H cell and treats the row as
  already-mapped, so it won't re-attempt or clobber it. It survives every future
  run and every re-ingest.

The manager who drafted the player keeps the slot but it now scores nothing.
Whether to allow a replacement pick is a **commissioner decision, separate from
the pipeline**.

> Dependency note: this works because the gate only requires column H to be
> non-empty. If that check is ever tightened to validate id *format* (e.g.
> `^pl_`), these sentinel rows must be revisited.

---

## Reconciliation check

`finished: N` in the report should equal the number of distinct matches in
`player_match_stats` once attention items are cleared. The ingest read line
(`player_match_stats: R existing rows across K matches`) is the running count —
after a successful write it should rise to match `finished`.

---

## Gotchas (the cycle-burners)

- **`OWN_GOAL_ALLOWANCE` must be edited in `ingest_match_stats.R`, never at the
  console.** Sourcing the file re-runs its own `OWN_GOAL_ALLOWANCE <- c(...)`
  line, overwriting any REPL assignment before the gate checks it. Tell: the
  mismatch line prints with no `+ own-goal allowance=` — that means the script
  saw an empty allowance. Edit the file, save, re-source.
- **Run from the project root.** All paths are relative; `Live fetch: NO` is the
  symptom of a wrong directory.
- **Manual columns** (`own_goals`, `penalty_saves`, shootout columns) are
  preserved on re-ingest — set them once in the Sheet and they survive.
- **No redeploy needed** — leaderboard reads `player_match_stats` via the
  5-minute cache.
- **The OWNED-UNMAPPED gate is team-level, not payload-level.** It fires for any
  owned player on either participating team with a blank column H — *whether or
  not they played this match*. So the named culprit may be a benched or absent
  player, not someone in the scoreline. Fix A (mappable), Fix C (withdrawn).
- **Backlog (data-in-code smell):** both `map_owned_overrides.R` aliases and the
  `OWN_GOAL_ALLOWANCE` entries live in code, so they reset on file edits and
  must be re-derived. Durable fix is to move both to small CSVs (or Sheet
  columns) the scripts read. Not urgent; flagged so it isn't lost.

---

## Living record — current overrides & allowances

Keep this in step with the code so a future "is this already handled?" is a
glance, not an investigation.

**Owned-player overrides** (`map_owned_overrides.R`):

| player_id | name | api_player_id | why auto-match failed |
|---|---|---|---|
| 142 | Danilo Luiz (BRA, DEF) | pl_9760160 | bare "Danilo"; two Danilos in squad |
| 144 | Gabriel (BRA, DEF) | pl_96242662 | "Gabriel" vs "Gabriel Magalhães"; multiple Gabriels |
| 767 | Yassine Bounou (MAR, GK) | pl_8642423 | nickname "Bono" |

**Own-goal allowances** (`OWN_GOAL_ALLOWANCE` in `ingest_match_stats.R`):

| match_id | match | own goal | own_goals cell set? |
|---|---|---|---|
| mt_517473281 | USA 4-1 PAR | Bobadilla (PAR), 7' | — |
| mt_641919660 | QAT 1-1 SUI | Muheim (SUI), 90+4' | set on Muheim's row |

**Withdrawn / non-participating owned players** (`WITHDRAWN` sentinel in column H):

| player_id | name | team | why |
|---|---|---|---|
| 524 | Lennart Karl | GER | withdrew from squad pre-tournament; no API record will ever exist |
