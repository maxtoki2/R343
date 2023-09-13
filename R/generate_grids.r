# generating all valid grids based on requirements (size and min combinations)
generate_valid_grids <- function(database_plr = plr_database, size = 3, min_combos = 5){
  inner_join(
    database_plr %>% filter(appearances > 0) %>% select(player_id, player_name, team_name)
    , database_plr %>% filter(appearances > 0) %>% select(player_id, player_name, team_name)
    , by = c("player_id", "player_name"), suffix = c("1", "2")
  ) %>%
    filter(team_name1 != team_name2) %>%
    distinct() -> combinations

  valid_combos <- combinations %>%
    group_by(team_name1, team_name2) %>%
    tally() %>%
    filter(n > min_combos) %>%
    select(-n) %>%
    setNames(c("team", "match"))

  combo_product <- reduce(
    .x = rep(list(valid_combos), size)
    , .f = inner_join
    , by = "match"
  ) %>%
    select(starts_with("team"), everything())

  combo_product <- combo_product[which(apply(combo_product %>% select(starts_with("team")), 1, function(x) length(unique(x))) == size), ]

  match_product <- purrr::reduce(
    .x = rep(list(combo_product), size)
    , .f = inner_join
    , by = setdiff(names(combo_product), "match")
  )

  valid_grids <- match_product[which(apply(match_product %>% select(starts_with("match")), 1, function(x) length(unique(x))) == size), ]
  valid_grids
}

create_daily_grid <- function(grid_population = valid_grids, database_plr = plr_database){
  immaculate_grid <- grid_population %>% sample_n(1)

  expand.grid(
    row = immaculate_grid %>% select(starts_with("team")) %>% unlist
    , column = immaculate_grid %>% select(starts_with("match")) %>% unlist
  ) -> grid_matrix

  valid_answers <- lapply(1:nrow(grid_matrix), function(i){
    row_players <- database_plr %>%
      filter(team_name == grid_matrix[i, "row"]) %>%
      distinct(player_id) %>%
      arrange(player_id)

    column_players <- database_plr %>%
      filter(team_name == grid_matrix[i, "column"]) %>%
      distinct(player_id) %>%
      arrange(player_id)

    inner_join(row_players, column_players, by = "player_id")
  })

  jolly_scores <- bind_rows(valid_answers) %>% group_by(player_id) %>% tally()

  list(grid_matrix, valid_answers, jolly_scores)
}
