library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyBS)
library(lubridate)
library(glue)
library(gt)
library(tidyr)
library(dplyr)
library(shinyWidgets)

#### TODO LIST
# point to github files
# remove ?/* from unknown first names
# settle on transliteration
# optimize input select
# players ungroup in the original file

rows <- rep(1:3, 3)
cols <- unlist(lapply(1:3, function(x) rep(x, 3)))
cells <- paste(rows, cols, sep = "_")

conditional_formatting <- tibble::tribble(
  ~guess, ~bgcolor, ~picture
  , 0, "white", "toguess.png"
  , 1, "green", "correct.png"
  , 2, "red", "gameover.png"
)

grid_file <- sort(grep("grid", list.files("../data-raw"), value = T), T)[1]
today_grid <- readRDS(glue("../data-raw/{grid_file}"))
names(today_grid[[2]]) <- cells
players <- readRDS("../data-raw/player_summary.RDS") %>%
  mutate(choice_txt = paste(player_name, time_range)) %>%
  arrange(player_name) %>%
  ungroup()
players_choices <- players$player_id
names(players_choices) <- players$choice_txt


ui <- fluidPage(
  useShinyjs()
  , gt_output(outputId = "table")
  , textOutput("dummy")
)



server <- function(input, output, session) {

  # playing_grid <- reactive({
  #   list(
  #     grid_matrix = today_grid[[1]]
  #
  #   )
  # })
  # previously_selected <- reactiveVal(list())
  # attempts_left <- reactiveVal(9)
  game_state <- reactiveValues(
    attempts_left = 9
    , players_used = character(0)
    # , guessed_right = matrix(0, 3, 3) #, dimnames = list(rows, cols)
    , cells_state = data.frame(
        cell = cells
        , guess = 0
        , correct_player = "---"
      )
  )

  # output$table <- render_gt(game_state$cells_state %>% gt())
  output$table <- render_gt({
    gtobj <- today_grid[[1]] %>%
      # mutate(dummy = "") %>%
      #mutate(rn = rows, cn = ) %>%
      #unite(cell, rn, cn) %>%
      mutate(cell = cells) %>%
      inner_join(game_state$cells_state) %>%
      inner_join(conditional_formatting) %>%
      mutate(cell_html = glue('<div id="cell{cell}">Text<br /><img src="{picture}" /><br />{correct_player}</div>')) %>%
      select(row, column, cell_html) %>%
      pivot_wider(names_from = column, values_from = cell_html) %>%
      gt(rowname_col = "row") %>%
      fmt_markdown(columns = -1) %>%
      tab_style(cell_text(size = 8), locations = cells_body())

    for(i in rows){
      for(j in cols){
        rc <- paste(i, j, sep = "_")
        cell_guess <- game_state$cells_state[which(game_state$cells_state$cell == rc), "guess"]
        gtobj <- gtobj %>%
          tab_style(style = list(
            cell_fill(color = case_when(
                cell_guess == 0 ~ "white"
                , cell_guess == 1 ~ "green"
                , cell_guess == 2 ~ "#D3D3D3"
              ))
            , cell_text(size = px(10))
          )

        , locations = cells_body(columns = j + 1, rows = i))
      }
    }

    gtobj
  })

  output$dummy <- renderText(game_state$attempts_left)

  lapply(cells, function(x){
    onclick(glue("cell{x}"), showModal(
      modalDialog(
        selectizeInput(glue("selPlayer{x}"), glue("Giocatore {x}"), players_choices[!(players_choices %in% game_state$players_used)])
        , footer = tagList(
            modalButton("Cancel")
            , actionButton(glue("actSelectPlayer{x}"), glue("OK ({x})"))
          )
      )
    ))

    observeEvent(input[[glue("actSelectPlayer{x}")]], {
      ci <- as.integer(unlist(strsplit(x, "_")))
      game_state$attempts_left <- game_state$attempts_left - 1
      sel_player <- input[[glue("selPlayer{x}")]]
      if(sel_player  %in% unlist(today_grid[[2]][x])){
        target_index <- which(game_state$cells_state$cell == x)
        game_state$players_used <- c(game_state$players_used, sel_player)
        game_state$cells_state[target_index, "guess"] <- 1
        game_state$cells_state[target_index, "correct_player"] <- players %>% filter(player_id == sel_player) %>% select(player_name) %>% unlist
      }
      if(game_state$attempts_left <= 0) game_state$cells_state[which(game_state$cells_state$guess == 0), "guess"] <- 2

      # game_state$players_used <- c(game_state$players_used, input[[glue("selPlayer{x}")]]) #TODO: only if guessed right
      # game_state$guessed_right[ci[1], ci[2]] <- 1 * (input[[glue("selPlayer{x}")]] %in% unlist(today_grid[[2]][x]))
      # if(game_state$attempts_left <= 0) game_state$guessed_right[which(game_state$guessed_right == 0)] <- 2
      removeModal()
    })
  })

}

shinyApp(ui, server)
