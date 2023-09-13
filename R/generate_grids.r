# generating all valid grids based on requirements (size and min combinations)
generate_valid_grids <- function(database_plr = plr_database, size = 3, min_combos = 5){
  inner_join(
    database_plr %>% filter(appearances > 0) %>% select(player_url, player_name, team_name)
    , database_plr %>% filter(appearances > 0) %>% select(player_url, player_name, team_name)
    , by = c("player_url", "player_name"), suffix = c("1", "2")
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

