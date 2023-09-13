# generate the historical database from the csv files
recreate_database <- function(country_selected, filepath){
  require(glue)
  available_files <- list.files(glue("{filepath}/{country_selected}"), full.names = T)

  lapply(available_files, function(f){
    read.csv(f)
  }) %>%
    bind_rows()
}

summarise_players <- function(database_plr = plr_database){
  # plr_database %>%
  database_plr %>%
    group_by(player_url, player_name, nationality) %>%
    summarise(min_season = min(season), max_season = max(season), app = sum(appearances), min = sum(minutes_played)) %>%
    unite(time_range, min_season, max_season, sep = "-")
}
