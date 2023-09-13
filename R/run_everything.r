# source("R/hello.R")
# hello()

x <- list.files()
write.table(x, file = paste0("data-raw/data_", make.names(Sys.time()), ".txt"))
