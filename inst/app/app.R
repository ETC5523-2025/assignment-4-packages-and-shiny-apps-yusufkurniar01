# inst/app/app.R
library(shiny)

# the data objects are part of the package namespace
# use :: to refer explicitly
sim_monthly <- yusufHAIGermany::sim_monthly
sim_weekly  <- yusufHAIGermany::sim_weekly
sim_daily   <- yusufHAIGermany::sim_daily

ui <- fluidPage(
  titlePanel("Germany HAI Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("freq", "Frequency", c("Monthly","Weekly","Daily"))
    ),
    mainPanel(tableOutput("peek"))
  )
)

server <- function(input, output, session) {
  dat <- reactive({
    switch(input$freq,
           "Monthly" = sim_monthly,
           "Weekly"  = sim_weekly,
           "Daily"   = sim_daily
    )
  })
  output$peek <- renderTable(head(dat(), 10))
}

shinyApp(ui, server)
