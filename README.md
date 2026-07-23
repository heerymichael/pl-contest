# World Cup 2026 Challenge

A Shiny app for a friends-and-family fantasy contest around the 2026 FIFA World Cup. Users build lineups of 11 players from the 48 qualified nations, with picks locked 1 hour before kickoff of the opening match. Scoring runs throughout the tournament.

## Status

**Phase 1 (complete):** lineup creation and editing, user accounts, lock-time handling.
**Phase 2:** visual identity.
**Phase 3:** post-lock stats collation, scoring, and leaderboard.

## Stack

- R / Shiny
- Google Sheets (via `googlesheets4`) as the data store for players, teams, entries, picks, contest config, and audit log
- Local SQLite database for user accounts, with `sodium` for password hashing

## Project layout

```
.
├── app.R                  # Main app entry point
├── R/
│   ├── auth_modals.R      # Signup/login modal flow
│   ├── cache.R            # 5-minute TTL cache over Sheets reads
│   ├── flags.R            # Country flag helpers
│   ├── lineup_creator.R   # Lineup picker (shared "new" and "edit" modes)
│   ├── lock.R             # Lock-time helpers, countdown banner
│   ├── my_lineups.R       # My Lineups list + embedded editor
│   ├── nav.R              # Nav header, Rules view, Leaderboard placeholder
│   ├── sheets.R           # Google Sheets data access layer
│   ├── users.R            # User registration, login, lookup (SQLite + sodium)
│   └── utils.R            # Shared helpers (e.g. %||%, wc_spinner)
├── www/
│   ├── styles.css         # All CSS lives here (no inline styles)
│   ├── spinner-loading.svg# Loading spinner asset (used via wc_spinner)
│   └── flags/             # SVG flags for the 48 qualified nations
├── data/
│   ├── teams_clean.csv    # 48 qualified nations
│   ├── players_clean.csv  # Player rosters
│   └── users_db.sqlite    # (Gitignored) user accounts
├── ingest_squads.R        # One-off: load teams/players CSVs into the Sheet
├── setup_users_db.R       # One-off: create the users SQLite database
├── service_account.json   # (Gitignored) Google service account credentials
├── .Renviron              # (Gitignored) environment variables
└── GUIDELINES.md          # Working guidelines for this project
```

## Environment variables

Set these in a local `.Renviron` at the project root (not committed):

| Variable | Purpose |
|---|---|
| `GS_AUTH_PATH` | Path to the Google service account JSON file |
| `GS_SHEET_ID` | Google Sheet ID for the contest data store |
| `USERS_DB_PATH` | Path to the users SQLite database (e.g. `data/users_db.sqlite`) |

## Running locally

1. Install R packages: `shiny`, `shinycssloaders`, `dplyr`, `tibble`, `lubridate`, `uuid`, `googlesheets4`, `googledrive`, `memoise`, `cachem`, `DBI`, `RSQLite`, `sodium`, `jsonlite`, `readr`.
2. Place the Google service account JSON at the path referenced by `GS_AUTH_PATH`.
3. Populate `.Renviron` with the variables above.
4. Run `source("setup_users_db.R")` once to create the users database.
5. Run `source("ingest_squads.R")` once to populate the `teams` and `players` tabs in the Google Sheet.
6. Launch the app: `shiny::runApp()`.

## Working guidelines

See [`GUIDELINES.md`](GUIDELINES.md) for the rules of engagement on this project — scope discipline, CSS conventions, diagnostics standards, etc.