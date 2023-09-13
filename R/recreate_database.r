# generate the historical database from the csv files
recreate_database <- function(country_selected, filepath){
  require(glue)
  available_files <- list.files(glue("{filepath}/{country_selected}"), full.names = T)

  lapply(available_files, function(f){
    read.csv(f)
  }) %>%
    bind_rows()
}
