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
  arrange(player_name)
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
    , guessed_right = matrix(0, 3, 3) #, dimnames = list(rows, cols)
    , cells_state = data.frame(
        cell = cells
        , guess = 0
      )
  )


  output$table <- render_gt({
    gtobj <- today_grid[[1]] %>%
      # mutate(dummy = "") %>%
      #mutate(rn = rows, cn = ) %>%
      #unite(cell, rn, cn) %>%
      mutate(cell = glue('<div id="cell{cells}">Text<br /><img src="toguess.png" /><br />Text</div>')) %>%
      pivot_wider(names_from = column, values_from = cell) %>%
      gt(rowname_col = "row") %>%
      fmt_markdown(columns = -1)

    for(i in rows){
      for(j in cols){
        gtobj <- gtobj %>%
          tab_style(style = cell_fill(color = case_when(
            game_state$guessed_right[i,j] == 0 ~ "white"
            , game_state$guessed_right[i,j] == 1 ~ "green"
            , game_state$guessed_right[i,j] == 2 ~ "black"
            )
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
      game_state$players_used <- c(game_state$players_used, input[[glue("selPlayer{x}")]])
      game_state$guessed_right[ci[1], ci[2]] <- 1 * (input[[glue("selPlayer{x}")]] %in% unlist(today_grid[[2]][x]))
      if(game_state$attempts_left <= 0) game_state$guessed_right[which(game_state$guessed_right == 0)] <- 2
      removeModal()
    })
  })

}

shinyApp(ui, server)
