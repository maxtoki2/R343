ui <- fluidPage(
  names(sessionInfo()$otherPkgs)
)
server <- function(input, output, session) {
}
shinyApp(ui, server)
