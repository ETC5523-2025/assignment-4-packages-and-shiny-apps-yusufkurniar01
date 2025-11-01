# inst/app/app.R
library(shiny)

# bring objects from the package into the app
sim_monthly <- get("sim_monthly", envir = asNamespace("yusufHAIGermany"))
sim_weekly  <- get("sim_weekly",  envir = asNamespace("yusufHAIGermany"))
sim_daily   <- get("sim_daily",   envir = asNamespace("yusufHAIGermany"))

ui <- fluidPage(
  titlePanel("Germany HAI Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("freq", "Frequency", c("Monthly","Weekly","Daily"))
    ),
    mainPanel(
      tableOutput("peek")
    )
  )
)

server <- function(input, output, session){
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
