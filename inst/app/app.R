# inst/app/app.R

#verify the launcher works
library(shiny)

ui <- fluidPage(
  titlePanel("Germany HAI Explorer"),
  sidebarLayout(
    sidebarPanel("Filters go here"),
    mainPanel(
      h4("Hello! Your app launched correctly."),
      plotOutput("demo")
    )
  )
)

server <- function(input, output, session) {
  output$demo <- renderPlot({
    plot(1:10, 1:10, main = "Demo plot")
  })
}

shinyApp(ui, server)
