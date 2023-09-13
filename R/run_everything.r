library(worldfootballR)
library(dplyr)
library(glue)
library(purrr)
library(tidyr)
library(stringi)

# TODO: update database

source("R/recreate_database.r")
source("R/generate_grids.r")

plr_database <- recreate_database("Italy", "inst/extdata")
plr_summary <- summarise_players(plr_database)

valid_grids <- generate_valid_grids(min_combos = 20)

daily_grid <- create_daily_grid(valid_grids, plr_database)

saveRDS(daily_grid, glue("data-raw/grid_{format(Sys.time(), '%Y%m%d')}.RDS"))

