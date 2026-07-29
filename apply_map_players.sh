#!/bin/bash
# Apply the api_player_id mapping package to the pl-contest repo.
# Run from anywhere: bash apply_map_players.sh
set -e
REPO="$HOME/Desktop/pl-contest"
SRC="$(cd "$(dirname "$0")" && pwd)"
[ -d "$REPO/R" ] || { echo "!! $REPO does not look like the pl-contest repo"; exit 1; }

cp "$SRC/map_players_2526.R"      "$REPO/map_players_2526.R"
cp "$SRC/snapshot_pl_reference.R" "$REPO/snapshot_pl_reference.R"
mkdir -p "$REPO/data"
cp "$SRC/api_id_overrides.csv"    "$REPO/data/api_id_overrides.csv"

FAIL=0
grep -q 'DRY_RUN <- TRUE' "$REPO/map_players_2526.R"            || { echo "!! DRY_RUN default missing"; FAIL=1; }
grep -q 'api_id_overrides.csv' "$REPO/map_players_2526.R"       || { echo "!! overrides wiring missing"; FAIL=1; }
grep -q 'pl_96242662' "$REPO/data/api_id_overrides.csv"         || { echo "!! Gabriel override missing"; FAIL=1; }
[ "$(tail -n +2 "$REPO/data/api_id_overrides.csv" | wc -l | tr -d ' ')" = "9" ] || { echo "!! overrides row count != 9"; FAIL=1; }
grep -q 'snapshot_pl_reference.R (v4)' "$REPO/snapshot_pl_reference.R" || { echo "!! snapshot not v4"; FAIL=1; }
grep -q 'left_join(sc_id, by = "api_player_id")' "$REPO/snapshot_pl_reference.R" || { echo "!! id join missing"; FAIL=1; }
grep -q 'chartr' "$REPO/map_players_2526.R" && grep -q 'chartr' "$REPO/snapshot_pl_reference.R" || { echo "!! normaliser fix missing"; FAIL=1; }
[ "$FAIL" -eq 1 ] && { echo "VERIFICATION FAILED"; exit 1; }

echo "DONE. Run order (in R, from the repo root):"
echo "  1. source(\"R/sheets.R\")             # auth"
echo "  2. source(\"map_players_2526.R\")     # DRY RUN — review output +"
echo "     /tmp/map_players_2526_review.csv"
echo "  3. edit DRY_RUN <- FALSE in map_players_2526.R, re-source     # writes + verifies"
echo "  4. source(\"snapshot_pl_reference.R\") # v4 snapshot"
echo "  5. restart R, run the app — Gabriel shows 329 in the pool"
