# inst/app/app.R
# Shiny dashboard for yusufHAIGermany
# Uses the package datasets shipped in data/: sim_daily, sim_weekly, sim_monthly

#load packages
library(shiny)
library(bslib)
library(plotly)
library(dplyr)
library(tidyr)
library(lubridate)
library(DT)
library(yusufHAIGermany)

#load data
data("sim_daily",   package = "yusufHAIGermany")
data("sim_weekly",  package = "yusufHAIGermany")
data("sim_monthly", package = "yusufHAIGermany")

#factor order for HAI types
hai_levels <- c("HAP","SSI","BSI","UTI","CDI")
for (x in c("sim_daily","sim_weekly","sim_monthly")) {
  df <- get(x)
  df$hai <- factor(df$hai, levels = hai_levels)
  assign(x, df, inherits = TRUE)
}

#date limits from my data
date_min <- min(sim_daily$date, sim_weekly$date, sim_monthly$date, na.rm = TRUE)
date_max <- max(sim_daily$date, sim_weekly$date, sim_monthly$date, na.rm = TRUE)

#helpers
agg_data <- function(freq, metric, hais, d_from, d_to) {
  # freq: "Daily" | "Weekly" | "Monthly"
  # metric: "Cases" | "Deaths" | "DALYs"
  # returns: data.frame(date, hai, value)
  metric_col <- switch(metric,
                       "Cases"  = c(day = "cases_day",  week = "cases_week",  month = "cases_month"),
                       "Deaths" = c(day = "deaths_day", week = "deaths_week", month = "deaths_month"),
                       "DALYs"  = c(day = "dalys_day",  week = "dalys_week",  month = "dalys_month")
  )

  if (freq == "Daily") {
    sim_daily |>
      filter(hai %in% hais, date >= d_from, date <= d_to) |>
      group_by(date, hai) |>
      summarise(value = sum(.data[[metric_col["day"]]], na.rm = TRUE), .groups = "drop") |>
      arrange(date)
  } else if (freq == "Weekly") {
    sim_weekly |>
      filter(hai %in% hais, date >= d_from, date <= d_to) |>
      group_by(date, hai) |>
      summarise(value = sum(.data[[metric_col["week"]]], na.rm = TRUE), .groups = "drop") |>
      arrange(date)
  } else {
    sim_monthly |>
      filter(hai %in% hais, date >= d_from, date <= d_to) |>
      group_by(date, hai) |>
      summarise(value = sum(.data[[metric_col["month"]]], na.rm = TRUE), .groups = "drop") |>
      arrange(date)
  }
}

kpi_cards <- function(df) {
  #define date, hai, value
  tibble(
    total = sum(df$value, na.rm = TRUE),
    peak  = max(df$value, na.rm = TRUE),
    peak_when = df$date[which.max(df$value)]
  )
}

#UI
ui <- page_navbar(
  title = "yusufHAIGermany — HAI Explorer",
  theme = bs_theme(
    version = 5,
    base_font = font_google("Inter"),
    heading_font = font_google("Inter"),
    primary = "#2C3E50"
  ),
  sidebar = sidebar(
    width = 320,
    h4("Filters"),
    radioButtons(
      "freq", "Frequency",
      choices = c("Monthly","Weekly","Daily"),
      selected = "Monthly", inline = TRUE
    ),
    selectInput(
      "metric", "Metric",
      choices = c("Cases","Deaths","DALYs"),
      selected = "Cases"
    ),
    checkboxGroupInput(
      "hais", "Infection types",
      choices = hai_levels, selected = hai_levels, inline = FALSE
    ),
    dateRangeInput(
      "dates", "Date range",
      start = date_min, end = date_max,
      min = date_min, max = date_max
    ),
    hr(),
    helpText("Tip: switch Frequency to change the time granularity.")
  ),

  nav(
    "Overview",
    layout_columns(
      col_widths = c(4,4,4),
      card(
        full_screen = TRUE,
        card_header("Total"),
        h2(textOutput("kpi_total"))
      ),
      card(
        full_screen = TRUE,
        card_header("Peak value"),
        h2(textOutput("kpi_peak"))
      ),
      card(
        full_screen = TRUE,
        card_header("Peak period"),
        h2(textOutput("kpi_when"))
      )
    ),
    card(
      full_screen = TRUE,
      card_header("Time-series"),
      plotlyOutput("ts", height = "420px")
    )
  ),

  nav(
    "Heatmap",
    card(
      full_screen = TRUE,
      card_header("Cases by week × HAI (for Weekly) / month × HAI (for Monthly)"),
      plotlyOutput("heat", height = "520px")
    )
  ),

  nav(
    "Table",
    card(
      full_screen = TRUE,
      card_header("Pivoted table (wide by HAI)"),
      DTOutput("table")
    )
  )
)

#server
server <- function(input, output, session) {

  filtered <- reactive({
    req(input$hais, input$dates)
    agg_data(
      freq = input$freq,
      metric = input$metric,
      hais = input$hais,
      d_from = input$dates[1],
      d_to   = input$dates[2]
    )
  })

  #KPIs
  observe({
    df <- filtered()
    k <- kpi_cards(df)
    output$kpi_total <- renderText(scales::comma(k$total))
    output$kpi_peak  <- renderText(scales::comma(k$peak))
    output$kpi_when  <- renderText(ifelse(is.finite(k$peak), format(k$peak_when, "%b %Y"), "—"))
  })

  #time series
  output$ts <- renderPlotly({
    df <- filtered()
    validate(need(nrow(df) > 0, "No data for the current filters."))

    plot_ly(
      df, x = ~date, y = ~value, color = ~hai,
      type = "scatter", mode = "lines",
      hovertemplate = paste0(
        "%{x|%Y-%m-%d}<br>",
        "%{fullData.name}: %{y:,.0f}<extra></extra>"
      )
    ) |>
      layout(
        yaxis = list(title = paste(input$metric)),
        xaxis = list(title = "Date"),
        legend = list(orientation = "h", x = 0, y = -0.2)
      )
  })

  #heatmap
  output$heat <- renderPlotly({
    req(input$freq %in% c("Weekly","Monthly"))
    metric <- input$metric

    if (input$freq == "Weekly") {
      metric_col <- switch(metric,
                           "Cases" = "cases_week", "Deaths" = "deaths_week", "DALYs" = "dalys_week"
      )
      hm <- sim_weekly |>
        filter(hai %in% input$hais, date >= input$dates[1], date <= input$dates[2]) |>
        mutate(week = isoweek(date), yr = year(date)) |>
        group_by(yr, week, hai) |>
        summarise(value = sum(.data[[metric_col]], na.rm = TRUE), .groups = "drop")
      plot_ly(hm, x = ~week, y = ~hai, z = ~value, type = "heatmap",
              colorscale = "Viridis",
              hovertemplate = "W%{x}, %{y}<br>%{z:,.0f}<extra></extra>") |>
        layout(xaxis = list(title = "ISO week"), yaxis = list(title = ""))
    } else {
      metric_col <- switch(metric,
                           "Cases" = "cases_month", "Deaths" = "deaths_month", "DALYs" = "dalys_month"
      )
      hm <- sim_monthly |>
        filter(hai %in% input$hais, date >= input$dates[1], date <= input$dates[2]) |>
        mutate(month = month(date)) |>
        group_by(month, hai) |>
        summarise(value = sum(.data[[metric_col]], na.rm = TRUE), .groups = "drop")
      plot_ly(hm, x = ~month, y = ~hai, z = ~value, type = "heatmap",
              colorscale = "Viridis",
              hovertemplate = "M%{x}, %{y}<br>%{z:,.0f}<extra></extra>") |>
        layout(xaxis = list(title = "Month (1–12)"), yaxis = list(title = ""))
    }
  })

  #pivoted table
  output$table <- renderDT({
    df <- filtered() |>
      tidyr::pivot_wider(names_from = hai, values_from = value)
    datatable(
      df,
      extensions = "Buttons",
      options = list(
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel"),
        pageLength = 12
      )
    )
  })
}

shinyApp(ui, server)
