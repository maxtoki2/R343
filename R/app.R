library(gt)

ui <- fluidPage(
  "Hello, world!"
  , gt_output(outputId = "table")
)
server <- function(input, output, session) {
  output$table <- render_gt({
    iris %>% gt()
  })
}
shinyApp(ui, server)
