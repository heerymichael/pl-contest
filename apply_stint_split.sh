#!/bin/bash
# Apply the mid-season mover stint split to the pl-contest repo.
# Run from anywhere: bash apply_stint_split.sh
set -e
REPO="$HOME/Desktop/pl-contest"
SRC="$(cd "$(dirname "$0")" && pwd)"
[ -d "$REPO/R" ] || { echo "!! $REPO does not look like the pl-contest repo"; exit 1; }

cp "$SRC/build_2526_scores.R" "$REPO/build_2526_scores.R"
cp "$SRC/scores_2526.R"       "$REPO/R/scores_2526.R"

# Stint tag styling — append once, guarded by marker
if ! grep -q 'scores-stint' "$REPO/www/styles.css"; then
cat >> "$REPO/www/styles.css" << 'CSS'

/* --- 25/26 exhibit: mid-season mover stint tag (v4 stint split) --- */
.scores-stint {
  margin-left: 6px;
  font-size: 11px;
  font-weight: 500;
  letter-spacing: 0.02em;
  color: var(--gaffer-muted, #8a8794);
  white-space: nowrap;
}
CSS
echo "   styles.css: stint tag CSS appended"
else
echo "   styles.css: stint tag CSS already present — untouched"
fi

FAIL=0
grep -q 'build_2526_scores.R (v4)' "$REPO/build_2526_scores.R"        || { echo "!! build not v4"; FAIL=1; }
grep -q 'pl2526_stints.rds' "$REPO/build_2526_scores.R"               || { echo "!! stints output missing"; FAIL=1; }
grep -q 'Stint rows do not sum to season totals' "$REPO/build_2526_scores.R" || { echo "!! stint gate missing"; FAIL=1; }
grep -q 'MID-SEASON MOVERS' "$REPO/build_2526_scores.R"               || { echo "!! mover report missing"; FAIL=1; }
grep -q 'recursive = FALSE' "$REPO/build_2526_scores.R"               || { echo "!! fixtures root-only guard missing"; FAIL=1; }
grep -q 'saveRDS(season, "data/pl2526_scores.rds")' "$REPO/build_2526_scores.R" || { echo "!! player-level rds output missing"; FAIL=1; }
grep -q 'pl2526_stints.rds' "$REPO/R/scores_2526.R"                   || { echo "!! module not reading stints"; FAIL=1; }
grep -q 'season_apps >= 4' "$REPO/R/scores_2526.R"                    || { echo "!! player-level floor missing"; FAIL=1; }
grep -q 'scores-stint' "$REPO/R/scores_2526.R"                        || { echo "!! stint tag render missing"; FAIL=1; }
grep -q 'scores-stint' "$REPO/www/styles.css"                         || { echo "!! stint tag CSS missing"; FAIL=1; }
[ "$FAIL" -eq 1 ] && { echo "VERIFICATION FAILED"; exit 1; }

echo "DONE. Run order (in R, from the repo root — no Sheets auth needed):"
echo "  1. source(\"build_2526_scores.R\")   # v4 — watch for the MID-SEASON"
echo "     MOVERS report and both consistency gate ticks"
echo "  2. restart R, run the app — Semenyo shows two rows (BOU + MCI)"
echo ""
echo "No snapshot rerun needed: pl2526_scores.rds values are unchanged."
