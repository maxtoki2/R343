library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyBS)
library(lubridate)
library(glue)
library(gt)
library(tidyr)
library(dplyr)

grid_file <- sort(grep("grid", list.files("../data-raw"), value = T), T)[1]
today_grid <- readRDS(glue("../data-raw/{grid_file}"))


ui <- fluidPage(
  useShinyjs()
  , gt_output(outputId = "table")
)



server <- function(input, output, session) {
  rows <- rep(1:3, 3)
  cols <- unlist(lapply(1:3, function(x) rep(x, 3)))
  cells <- paste(rows, cols, sep = "_")

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

  lapply(glue("cell{cells}"), function(x){
    onclick(x, showModal(modalDialog(
      title = "Your title",
      renderDataTable(data)
    )))
  })

}

shinyApp(ui, server)
