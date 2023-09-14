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

# ui <- tagList(
#   useShinyjs(),
#   dashboardPage(
#     dashboardHeader(title = "Telemedicine HP"),
#     dashboardSidebar(),
#     dashboardBody(
#       # column(
#       #   width = 6
#       #   , infoBox("", "", "", icon("fa-solid fa-futbol"))
#       #   , valueBoxOutput("C1")
#       #   , valueBoxOutput("C2")
#       #   , valueBoxOutput("C3")
#       # )
#       fluidRow(
#         # empty box then 3 column headers
#         infoBox("", "", "", icon("fa-solid fa-futbol"), width = 1)
#         , valueBoxOutput("C1", width = 1)
#         , valueBoxOutput("C2", width = 1)
#         , valueBoxOutput("C3", width = 1)
#       )
#       #
#       , fluidRow(
#         valueBoxOutput("R1", width = 1),
#         # valueBox(100, "Astutillo Malgioglio", color = "aqua", width = 1),
#         # box(background = "red", width = 2, height = 100),
#         div(id='R1C1',
#             valueBox(60, subtitle = "test player", icon = icon("trademark"), color = "purple", width = 1, href = NULL)
#         )
#       )
#     )
#   )
# )

ui <- fluidPage(
  useShinyjs()
  , gt_output(outputId = "table")
)

# server <-  function(input, output, session){
#   # output$R1 <- renderValueBox({
#   #   valueBox("", today_grid[[1]][1, "row"], color = "aqua", width = 1)
#   # })
#
#   output$C1 <- renderValueBox({
#     valueBox("", today_grid[[1]][1, "column"], color = "aqua", width = 1)
#   })
#
#   output$C2 <- renderValueBox({
#     valueBox("", today_grid[[1]][4, "column"], color = "aqua", width = 1)
#   })
#
#   output$C3 <- renderValueBox({
#     valueBox("", today_grid[[1]][7, "column"], color = "aqua", width = 1)
#   })
#
#   onclick('R1C1', showModal(modalDialog(
#     title = "Your title",
#     renderDataTable(data)
#   )))
# }

server <- function(input, output, session) {
  output$table <- render_gt({
    today_grid[[1]] %>%
      # mutate(dummy = "") %>%
      mutate(rn = rep(1:3, 3), cn = unlist(lapply(1:3, function(x) rep(x, 3)))) %>%
      unite(cell, rn, cn) %>%
      mutate(cell = glue('<div id="cell{cell}">text</div>')) %>%
      pivot_wider(names_from = column, values_from = cell) %>%
      gt(rowname_col = "row") %>%
      fmt_markdown(columns = -1)
  })

  onclick('cell1_1', showModal(modalDialog(
      title = "Your title",
      renderDataTable(data)
    )))
}

shinyApp(ui, server)
