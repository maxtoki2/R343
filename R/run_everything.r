library(worldfootballR)
library(dplyr)
library(glue)
library(purrr)
library(tidyr)

# TODO: update database

source("R/recreate_database.r")
source("R/generate_grids.r")

plr_database <- recreate_database("Italy", "inst/extdata")

valid_grids <- generate_valid_grids(min_combos = 20)

write.csv(nrow(valid_grids), file = "data-raw/gridsize.txt")
