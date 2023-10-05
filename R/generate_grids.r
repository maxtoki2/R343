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

generate_daily_grid <- function(database_plr = plr_database, size = 3, min_combos = 5){
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

  valid_grids <- valid_combos
  valid_grids <- valid_grids %>% apply(1, sort) %>% t %>% `colnames<-`(colnames(valid_grids)) %>% as.data.frame() %>% distinct()

  while(ncol(valid_grids) < size + 1){
    current_ncol <- ncol(valid_grids)
    names(valid_grids)[current_ncol] <- paste0("match", current_ncol - 1)

    valid_grids <- valid_grids %>%
      inner_join(valid_combos, by = "team")

    valid_grids <- valid_grids[which(apply(valid_grids, 1, function(x) length(unique(x))) == ncol(valid_grids)), ]

    valid_grids <- cbind(team = valid_grids[,1], valid_grids[,-1] %>% apply(1, sort) %>% t %>% `colnames<-`(colnames(valid_grids)[-1])) %>% as.data.frame() %>% distinct()

  }

  names(valid_grids)[size + 1] <- paste0("match", size)

  grids_with_enough_combos <- valid_grids %>%
    group_by_at(vars(starts_with("match"))) %>%
    tally() %>%
    filter(n >= size) %>%
    ungroup() %>%
    select(-n)

  selected_combo <- grids_with_enough_combos %>% sample_n(1)

  selected_grid_long <- selected_combo %>%
    inner_join(valid_grids) %>%
    sample_n(size)

  immaculate_grid <- selected_grid_long %>%
    mutate(dummy = 1:3) %>%
    pivot_wider(names_from = dummy, values_from = team, names_prefix = "team") %>%
    select(starts_with("team"), everything())

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

  jolly_scores <- bind_rows(valid_answers) %>% group_by(player_id) %>% tally() %>% filter(n > 2)

  list(grid_matrix, valid_answers, jolly_scores)

}
