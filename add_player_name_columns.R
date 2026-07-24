# add_player_name_columns.R ----------------------------------------------
# Adds first_name and surname columns to the `players` tab, joined from
# the original FanTeam export on fanteam_id. Needed because the pool UI
# renders surname (heavy) + first name (light), and splitting the
# display name on the last word breaks multi-word surnames (van Dijk,
# De Bruyne).
#
# player_id values are READ from the tab and written back unchanged —
# ID stability is preserved exactly regardless of row order.
#
# Safe to re-run (idempotent). After running, re-run
# snapshot_pl_reference.R and restart the app.
#
#   source("R/sheets.R")
#   source("add_player_name_columns.R")

library(dplyr)
library(readr)
library(googlesheets4)

message(">>> add_player_name_columns — target Sheet: ", sheet_id())

CSV_PATH <- "data/fanteam_players_2627.csv"
stopifnot(file.exists(CSV_PATH))

players <- read_sheet(sheet_id(), sheet = "players",
                      col_types = "c") |>
  mutate(player_id     = as.integer(player_id),
         fanteam_price = as.numeric(fanteam_price))
message("    [names] read players tab — ", nrow(players), " rows")

ft <- read_csv(CSV_PATH,
               col_types = cols(.default = col_character())) |>
  transmute(fanteam_id = PlayerID,
            first_name = trimws(FName),
            surname    = trimws(Name)) |>
  # Doubled-name halving mirror: where first == surname, blank the
  # first name so the UI shows the single form heavy.
  mutate(first_name = ifelse(tolower(first_name) == tolower(surname),
                             "", first_name))

out <- players |>
  select(-any_of(c("first_name", "surname"))) |>
  left_join(ft, by = "fanteam_id")

n_miss <- sum(is.na(out$surname))
if (n_miss > 0) {
  stop(">>> [names] ", n_miss, " players failed to join on fanteam_id — ",
       "investigate before writing", call. = FALSE)
}

googlesheets4::sheet_write(out, ss = sheet_id(), sheet = "players")
message(">>> add_player_name_columns complete — players tab now has ",
        ncol(out), " columns (first_name, surname appended). ",
        "Re-run snapshot_pl_reference.R next.")
