ui <- fluidPage(
  sessionInfo()
)
server <- function(input, output, session) {
}
shinyApp(ui, server)
