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

grid_file <- sort(grep("grid", list.files("../data-raw"), value = T), T)[1]
today_grid <- readRDS(glue("../data-raw/{grid_file}"))
players <- readRDS("../data-raw/player_summary.RDS") %>%
  mutate(choice_txt = paste(player_name, time_range)) %>%
  arrange(player_name)
  # TODO: remove */? and decide on transliteration
players_choices <- players$player_id
names(players_choices) <- players$choice_txt


ui <- fluidPage(
  useShinyjs()
  , gt_output(outputId = "table")
  , textOutput("dummy")
)



server <- function(input, output, session) {
  rows <- rep(1:3, 3)
  cols <- unlist(lapply(1:3, function(x) rep(x, 3)))
  cells <- paste(rows, cols, sep = "_")

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
  )


  output$table <- render_gt({
    today_grid[[1]] %>%
      # mutate(dummy = "") %>%
      #mutate(rn = rows, cn = ) %>%
      #unite(cell, rn, cn) %>%
      mutate(cell = glue('<div id="cell{cells}">Text<br /><img src="toguess.jpg" /><br />Text</div>')) %>%
      pivot_wider(names_from = column, values_from = cell) %>%
      gt(rowname_col = "row") %>%
      fmt_markdown(columns = -1)
  })

  output$dummy <- renderText(paste(game_state$players_used, collapse = ","))

  lapply(cells, function(x){
    onclick(glue("cell{x}"), showModal(
      modalDialog(
        selectInput("selPlayer", glue("Giocatore {x}"), players_choices[!(players_choices %in% game_state$players_used)])
        , footer = tagList(
            modalButton("Cancel")
            , actionButton(glue("actSelectPlayer{x}"), glue("OK ({x})"))
          )
      )
    ))
  })

  lapply(cells, function(x){
    observeEvent(input[[glue("actSelectPlayer{x}")]], {
      game_state$attempts_left <- game_state$attempts_left - 1
      game_state$players_used <- c(game_state$players_used, input$selPlayer)
    })
  })


}

shinyApp(ui, server)
