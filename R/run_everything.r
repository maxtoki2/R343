library(dplyr)

source("R/recreate_database.r")

plr_database <- recreate_database("Italy", "inst/extdata")

write.csv(nrow(plr_database), file = "data-raw/dbsize.txt")
