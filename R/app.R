library(ggplot2)
library(dplyr)

ui <- fluidPage(
  "Hello, world!"
  # , gt_output(outputId = "table")
  , plotOutput("explot")
)
server <- function(input, output, session) {
  # output$table <- render_gt({
  #   iris %>% gt()
  # })
  output$explot <- renderPlot(
    iris %>%
      ggplot(aes(x = Sepal.Length, y = Sepal.Width, col = Species)) +
      geom_point()
  )
}
shinyApp(ui, server)
