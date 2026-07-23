# Working Guidelines — World Cup 2026 Challenge

These guidelines govern how Claude works on this project. They are the source of
truth. If any instruction elsewhere in a conversation conflicts with this
document, this document wins unless we have explicitly agreed to deviate (see
rule 8).

## 1. Stick to the guidelines

Always work within these guidelines. The only exception is rule 8: if following
a guideline would cause a real bug or break the app, stop and flag it rather
than either following blindly or quietly deviating.

## 2. Do nothing beyond what is explicitly agreed

No unrequested features, validation, error handling, refactors, or
"while I'm here" cleanup. Out-of-scope ideas are surfaced as suggestions for
Michael to accept or reject, never baked into the work.

## 3. Never undo anything without explicit confirmation

This includes deleting code, removing functions, replacing existing patterns,
or reverting prior decisions — even where the change looks obviously
beneficial. Flag and ask first.

## 4. No duplicated CSS, no inline CSS

Styling lives in a single canonical file: **`www/styles.css`**, loaded once in
`app.R` via `tags$head(tags$link(rel = "stylesheet", type = "text/css",
href = "styles.css"))`.

All styling lives there. No `style="..."` attributes on individual elements.
No repeating the same rules across files. New UI elements get a semantic class
added to `styles.css`, never inline styles.

The current `styles.css` is deliberately functional/plain — full visual
identity is a later pass once the functional surfaces are all built.

## 5. One step at a time

When working through a process, do one step, stop, and wait for confirmation
before the next. Do not chain steps.

## 6. No long unprompted runs

Do not launch into long sequences of work without first laying out the steps
and getting agreement. Multi-step plans are presented for sanction before any
file is written.

## 7. Show before applying

For any non-trivial edit, show the proposed change (or a diff) before writing
to disk. Trivial here means typo fixes or single-line corrections previously
discussed; anything else gets shown first.

## 8. Flag rather than break

If following these guidelines would cause a real bug, leave the app in a
broken state, or contradict something we just agreed, stop and flag it. Do not
silently deviate, and do not follow blindly into a known breakage.

## 9. No silent assumptions

If a request is ambiguous, ask rather than guess — even if guessing would be
faster.

## 10. Scope discipline

Only touch files relevant to the current task. If a change in file X appears
to require a change in file Y, surface that and ask before widening scope.

## 11. Preserve existing patterns

Where the codebase has an established convention, match it rather than
introducing a new style. Current conventions include:

  - `snake_case` for function and variable names.
  - The `%||%` null-coalescing helper from `utils.R`.
  - Environment variables for secrets and paths
    (`SHINYMANAGER_DB_PATH`, `GS_AUTH_PATH`, `GS_SHEET_ID`, etc.).
  - `reactiveVal` for app-level state in `app.R`.
  - `source("R/...")` for module loading.
  - View functions return a `div(...)` or `tagList(...)`; UI shells live in
    `nav.R` and `app.R`.
  - All CSS lives in `www/styles.css` (see rule 4).
  - External-service URLs (Teamstake, etc.) live as constants in
    `R/utils.R` with a paired helper function (e.g. `TEAMSTAKE_URL` +
    `teamstake_link()`). No hardcoded service URLs in views or modules.
  - Responsive design uses two canonical breakpoints: `≤ 768px`
    (mobile/tablet) and `≤ 480px` (narrow phones). New responsive
    rules should use these, not introduce new ones.
  - Any `:hover` rule on interactive elements is wrapped in
    `@media (hover: hover) { ... }` so the hover state doesn't stick
    on touch devices after a tap.

## 12. Updates to these guidelines

Whenever we agree something that either (a) contravenes an existing rule, or
(b) creates the need for a new rule, an updated `GUIDELINES.md` is provided as
a matter of course — not on request. The updated file is the new source of
truth from that point on.

## 13. Prefer existing R packages; stay in R by default

Before writing custom code, check whether a reliable, well-maintained R
package already does the job. Before reaching beyond R into other languages
or systems (Python, JavaScript, shell, external services), check whether
there is an R-based solution that meets the need.

"Reliable and robust" means actively maintained, reasonably popular, and
suited to the actual task — not just the first package that turns up. If no
suitable R package exists, surface that finding and the proposed alternative
before proceeding.

## 14. Diagnostics by default

New code includes `message()` instrumentation from the start, not added
retroactively when something breaks. The aim is to shorten the debug loop —
when something doesn't work, the console already tells us what ran and in
what order.

Minimum standard for any new `.R` file or function:

  - **Sourcing marker** at the top of each `.R` file:
    `message(">>> SOURCED R/<filename>.R at ", format(Sys.time()))`
  - **Entry marker** at the start of each server-logic function:
    `message(">>> ENTER <function_name>")`
  - **Outcome logs** around any external-system call (Sheets, users DB,
    file I/O) — log the inputs going in and a short summary of what came
    back (row count, success/failure, error message). No sensitive data
    (passwords, full email lists) in logs.

Diagnostics are on by default during development. If/when we deploy, we'll
add a simple `DEBUG` flag to gate the verbose ones.

Existing files get the markers retrofitted opportunistically — only when
that file is being touched for another reason (rule 10 still applies).

---

*Last updated: 23 May 2026 — added rule 14 (diagnostics by default); deleted unused `lineup_creator_view()` placeholder from `nav.R`; extended rule 11 conventions with external-URL helpers, mobile breakpoints, and hover-scoping.*