# Transfer Window — Runbook

The Gameweek 3 transfer window: what to change, in what order, and how to
verify each step before moving on. Written to be followed cold.

**The rule:** each entry may change up to 2 of its 15 players during a single
window, open **Thu 3 Sep 09:00** to **Fri 4 Sep 19:00** (Europe/London),
closing before the first Gameweek 3 kickoff. Changes take effect from
Gameweek 3; points banked in GW1–2 by an outgoing player stand.

**Why it exists:** entries lock at the opening kickoff on 21 August, but the
summer transfer window doesn't shut until 1 September. This is a narrow
chance to reflect the real-world moves that happen in between — not a free
hit.

---

## Open decisions

Resolve these before writing code. Defaults are what the plan currently
assumes; strike through and replace as they're confirmed.

| # | Decision | Assumed default |
|---|---|---|
| 1 | Per entry or per user? | **Per entry** — 5 entries = 5 × 2 transfers |
| 2 | Effective gameweek | **GW3**; outgoing keeps GW1–2 points |
| 3 | Budget accounting | **Derived** from set difference vs. locked roster. A→B→A back = 0 used |
| 4 | Do club-clash auto-subs count? | **No** — forced, not chosen |
| 5 | Refresh pool for deadline-day arrivals? | **Yes** — see Phase 3 |
| 6 | Window datetimes | **Thu 3 Sep 09:00 / Fri 4 Sep 19:00, Europe/London** |

---

## The `period` model

`roster_picks` already carries a `period` column (created by
`setup_pl_sheet.R`, never populated by the app).

**Semantics: full roster per period.** Every period holds a complete 15-row
roster for each entry.

| Period | Gameweeks |
|---|---|
| 1 | GW1–2 |
| 2 | GW3–19 |

Roster for a gameweek = rows where `period == period_for_gw(gw)`. An entry
that makes no transfers still gets 15 fresh period-2 rows, 13 of them
identical to period 1. This costs ~3,000 duplicate rows and buys a one-line
filter in scoring, permanent history of every roster state, and the same
mechanism for the automatic club-clash substitutions.

Period 1 rows are **never** modified or deleted after backfill.

---

## Phase 0 — before lock (21 Aug)

**Nothing to do.** No code change, no redeploy, no Sheet edit. Entries are
still being written right up to kickoff; anything done now goes stale.

---

## Phase 1 — backfill `period` (any time after lock)

`roster_picks` is frozen once the contest locks — every write path
(`submit_lineup`, `confirm_submit_lineup`, `confirm_save_lineup`) fresh-reads
`is_locked()` and bails. Safe to edit by hand.

1. Open the `roster_picks` tab in `pl-contest-data`.
2. Select column F (`period`) from row 2 to the last data row.
3. Enter `1` and fill down.

**Cautions:** the cells must be *numbers*, not text — no leading apostrophe,
and check the cell format isn't set to Plain text. Do not sort or reorder
rows.

**Verification gate — all three must pass before continuing:**

- Blank cells in `period`: **0**
- Distinct values in `period`: **{1}** only
- Row count == 15 × (number of non-void entries)

Spot-check one entry: filter by a known `entry_id`, confirm exactly 15 rows,
all period 1, positions summing to 2 GK / 5 DEF / 5 MID / 3 FWD.

---

## Phase 2 — code changes

Each batch is self-contained and gets committed on its own once verified.
Full files, not diffs.

### Batch 1 — config and time model

| File | Change |
|---|---|
| `add_transfer_config.R` *(new)* | Adds `transfers_open_at`, `transfers_close_at`, `transfers_max`, `transfer_effective_gw` to `contest_config`. Same shape as `fix_pl_config2.R` — wide one-row, datetimes written as POSIXct so they read back as POSIXct |
| `R/transfers.R` *(new)* | `transfer_window_state()` fresh-read + `transfer_window_state_cached()`, mirroring the `is_locked()` / `is_locked_cached()` split in `R/lock.R`. Returns `"before"` / `"open"` / `"closed"` |
| `app.R` | `source("R/transfers.R")` |

**Verify:** `get_contest_config()` returns POSIXct (not character) for both
datetimes. `transfer_window_state()` returns `"before"` today.

### Batch 2 — rules page

| File | Change |
|---|---|
| `R/nav.R` | New high-attention section; **three existing passages amended** — see below |
| `www/styles.css` | `.panel-card-alert` (2px magenta border), `.rules-flag` (RULE CHANGE pill) |

Three passages currently contradict the new rule and must be edited, not just
added to:

- **Bestball** — "no transfers"
- **Transfers after lock (Gameweeks 1–3)** — implies the club-clash swap is
  the only roster change
- **Withdrawals** — "the only exception is the automatic club-clash
  substitution"

Also amend the **player pool** section if decision 5 lands on "yes" — it
currently states the pool is frozen at lock.

### Batch 3 — banner

| File | Change |
|---|---|
| `R/lock.R` | `countdown_banner_ui()` becomes phase-aware. Renders message + optional secondary line + `data-until` |
| `app.R` | Countdown JS reads `data-until` off the element rather than a single hardcoded `lockAtMs`; fires a phase-change event on zero. R bumps a tick and re-renders |
| `www/styles.css` | `.countdown-banner-secondary`; message/timer wrapped in a row div |

Phases: pre-lock (lock countdown, transfer-window notice as second line) →
post-lock (countdown to window opening) → open (countdown to close, magenta
band) → closed (static notice).

### Batch 4 — period-aware reads

| File | Change |
|---|---|
| `R/transfers.R` | `period_for_gw()`, driven by `transfer_effective_gw` |
| `R/bestball.R` | `bestball_gameweek_points()` filters roster by period per gameweek |
| `R/sheets.R` | `get_picks_for_entry()` returns the entry's highest present period. `save_roster_picks()` takes a `period` argument (default 1) and writes column F |
| `R/cache.R` | `get_picks_for_entry_local()` matches the same period logic |

**Cast before comparing** — Sheets returns numbers as float (`1.0`). Use
`as.integer()` on `period` at every read site.

**Verify against period-1-only data:** leaderboard totals must be *identical*
before and after this batch. Any drift means the filter is wrong.

### Batch 5 — the write path

| File | Change |
|---|---|
| `R/sheets.R` | `apply_transfers(entry_id, new_picks)` — fresh window-state check, delta vs. locked baseline, budget guard, roster-validity guard, appends 15 period-2 rows, bumps `updated_at`, `log_action()` |
| `R/sheets.R` | `update_entry_picks()` scopes its delete-and-rewrite to the entry's *current period only* |

The `update_entry_picks()` guard is not optional. It currently rewrites the
whole tab minus the entry's rows; once period 2 exists, an unscoped rewrite
silently deletes GW1–2 history.

**Concurrency note:** the wipe-and-rewrite pattern is a read-modify-write over
the whole tab. Two saves in the same second can clobber each other. Low odds
across a 34-hour window with ~50 entrants, but `apply_transfers()` should
append rather than rewrite wherever possible.

**Dry-run first.** `DRY_RUN <- TRUE` in the file default; run against a
duplicate of the Sheet before touching live data.

### Batch 6 — editor UI

| File | Change |
|---|---|
| `R/lineup_creator.R` | Separate "can edit" reactive from `locked` so the edit picker opens while the contest stays locked for new entries. Transfer counter, changed players highlighted, budget-aware submit |
| `R/my_lineups.R` | Passes the new reactive through |
| `www/styles.css` | Transfer counter, changed-pick highlight |

The picker currently keys ~10 UI decisions off `is_locked_reactive()`. In
`mode = "new"` the new reactive is `!locked`; in `mode = "edit"` it's
`!locked || window_open`.

---

## Phase 3 — pool refresh and redeploy (1–3 Sep)

Only if decision 5 is **yes**.

1. Re-run `refresh_fanteam_players.R` to pull post-deadline-day club listings.
2. Re-run `map_players_2526.R` for any new arrivals needing `api_player_id`.
3. Re-run `snapshot_pl_reference.R` — `players_snapshot.rds` is baked into the
   deploy, so without this, deadline-day arrivals are unselectable.
4. Deploy.

**Verify:** a known deadline-day signing appears in the pool at his new club,
and his old club's slot is still fillable.

---

## Phase 4 — window open (3–4 Sep)

**The snapshot trap.** Post-lock the leaderboard and editor read
`data/entries_snapshot.rds` and `data/roster_picks_snapshot.rds` from the
bundle. If picks change while those are live, everyone sees pre-transfer
rosters.

For the duration of the window the app **must** fall back to live Sheets
reads. Either ship without the picks snapshot, or gate the local read on
`transfer_window_state() != "open"`.

Spot-check within the first hour: make a transfer on a test entry, confirm 15
period-2 rows appear, period-1 rows untouched, and the editor reloads showing
the new roster.

---

## Phase 5 — window closes (after 19:00, 4 Sep)

1. Confirm no further writes — `transfer_window_state()` returns `"closed"`.
2. Re-run `snapshot_contest_data.R` to capture post-transfer rosters.
3. Redeploy.
4. Verify the leaderboard renders GW1–2 points from period-1 rosters and GW3
   onward from period-2.

**Entries that made no transfers still need 15 period-2 rows.** If
`apply_transfers()` only ever ran for entries that changed something, every
other entry scores 0 from GW3. Backfill period 2 for untouched entries as
part of closing the window, and verify: distinct entries with period-2 rows
== total non-void entries.

---

## Rollback

Period-1 rows are never modified, so GW1–2 scoring is always recoverable.

To unwind a bad period-2 write: delete every row where `period == 2` for the
affected entry and re-run. To unwind the whole window: delete all period-2
rows; scoring reverts to period-1 rosters for every gameweek.

Take a Sheet copy (File → Make a copy) before Phase 1 and again before
Phase 5.

---

## Verification queries

```r
source("R/sheets.R")
picks <- read_sheet_tab("roster_picks")

# Phase 1 gate
sum(is.na(picks$period))                    # expect 0
table(picks$period, useNA = "ifany")        # expect 1 only

# Roster integrity per period
library(dplyr)
picks |>
  mutate(period = as.integer(period)) |>
  count(entry_id, period) |>
  filter(n != 15)                           # expect 0 rows

# Phase 5 gate — every entry has a period-2 roster
entries <- read_sheet_tab("entries")
setdiff(entries$entry_id,
        unique(picks$entry_id[as.integer(picks$period) == 2]))  # expect empty
```

---

*Last updated: 19 Aug 2026 — initial draft. Decisions 1–6 unconfirmed.*
