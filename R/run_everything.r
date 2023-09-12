# source("R/hello.R")
# hello()

x <- glue("Hello, world! Today is {Sys.Date()}")
save(x, file = paste0("data-raw/data_", make.names(Sys.time()), ".Rda"))
