# yusufHAIGermany — HAI Explorer

# packages
library(shiny)
library(bslib)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(lubridate)
library(scales)
library(glue)
library(shinyjs)

# data
load("data/sim_daily.rda",   envir = globalenv())
load("data/sim_weekly.rda",  envir = globalenv())
load("data/sim_monthly.rda", envir = globalenv())

stopifnot(exists("sim_daily"), exists("sim_weekly"), exists("sim_monthly"))

hai_levels <- c("HAP","SSI","BSI","UTI","CDI")
for (x in c("sim_daily","sim_weekly","sim_monthly")) {
  df <- get(x)
  df$hai <- factor(df$hai, levels = hai_levels)
  assign(x, df, inherits = TRUE)
}

sex_levels <- c("Female","Male")
date_min <- min(sim_daily$date, sim_weekly$date, sim_monthly$date, na.rm = TRUE)
date_max <- max(sim_daily$date, sim_weekly$date, sim_monthly$date, na.rm = TRUE)

# helpers
pick_metric_cols <- function(metric) {
  switch(metric,
         "Cases"  = c(day="cases_day",   week="cases_week",   month="cases_month"),
         "Deaths" = c(day="deaths_day",  week="deaths_week",  month="deaths_month"),
         "DALYs"  = c(day="dalys_day",   week="dalys_week",   month="dalys_month")
  )
}

agg_data <- function(freq, metric, hais, sexes, d_from, d_to) {
  cols  <- pick_metric_cols(metric)
  get_df <- switch(freq, "Daily" = sim_daily, "Weekly" = sim_weekly, sim_monthly)
  idx    <- switch(freq, "Daily" = "day", "Weekly" = "week", "Monthly" = "month")

  get_df |>
    filter(hai %in% hais, sex %in% sexes, date >= d_from, date <= d_to) |>
    group_by(date, region, sex, hai) |>
    summarise(value = sum(.data[[cols[[idx]]]], na.rm = TRUE), .groups = "drop") |>
    arrange(date, region, sex, hai)
}

facet_apply <- function(p, facet_by) {
  if (facet_by == "Region")       return(p + facet_wrap(~ region, scales = "free_y", ncol = 2))
  if (facet_by == "Sex")          return(p + facet_wrap(~ sex,    scales = "free_y", ncol = 2))
  if (facet_by == "Region × Sex") return(p + facet_grid(sex ~ region, scales = "free_y"))
  p
}

# automatic insight
insight_text <- function(df, metric, facet_by) {
  by_hai <- df |>
    group_by(hai) |>
    summarise(total = sum(value, na.rm = TRUE), .groups = "drop") |>
    arrange(desc(total))

  top <- by_hai$hai[1]; top_v <- by_hai$total[1]
  bot <- by_hai$hai[nrow(by_hai)]; bot_v <- by_hai$total[nrow(by_hai)]
  peak_row  <- df[which.max(df$value), , drop = FALSE]
  peak_when <- if (nrow(peak_row)) format(peak_row$date, "%b %Y") else "—"

  facet_note <- switch(
    facet_by,
    "Region"       = "Figures are split by region.",
    "Sex"          = "Figures are split by sex.",
    "Region × Sex" = "Figures are split by region and sex.",
    "None"         = "Figures are not split by facet."
  )

  glue(
    "{metric}: {top} carries the largest share ({comma(top_v)}) while {bot} is the smallest ({comma(bot_v)}). ",
    "The highest single period occurs in {peak_when}. ",
    "{facet_note}"
  )
}

# overall summary (≤3 sentences)
overall_text <- function(df, metric) {
  by_hai <- df |> group_by(hai) |>
    summarise(total = sum(value, na.rm = TRUE), .groups = "drop") |>
    arrange(desc(total))
  top <- by_hai$hai[1]; bot <- by_hai$hai[nrow(by_hai)]

  early <- df |> filter(date <= min(date) + 60) |> summarise(v = sum(value, na.rm = TRUE)) |> pull(v)
  late  <- df |> filter(date >= max(date) - 60) |> summarise(v = sum(value, na.rm = TRUE)) |> pull(v)
  dir   <- ifelse(late >= early, "slightly higher toward the end", "slightly lower toward the end")

  glue(
    "{metric}: {top} dominates overall and {bot} contributes the least. ",
    "Totals are {dir} of the selected window. ",
    "Peak activity is concentrated in a few months rather than spread evenly."
  )
}

#UI
ui <- page_navbar(
  id = "nav",    #track active tab
  title = "yusufHAIGermany — HAI Explorer",
  theme = bs_theme(
    version = 5,
    base_font    = font_google("Inter"),
    heading_font = font_google("Inter"),
    primary = "#2C3E50",
    bg = "#f6f7fb",
    fg = "#212529"
  ),

  useShinyjs(),

  #compact sidebar, smaller dropdown options, and tight summary spacing
  tags$style(HTML("
    .sidebar { font-size:12px; }
    .sidebar h4{ font-size:14px; margin-bottom:6px;}
    .sidebar .form-label{ font-size:12px; margin-bottom:4px;}
    .sidebar .form-control, .sidebar .form-select{ font-size:12px; padding:4px 8px;}
    .sidebar .form-check-label{ font-size:12px;}
    .sidebar .form-check{ margin-bottom:4px;}
    .sidebar .mb-3, .sidebar .form-group{ margin-bottom:8px !important;}
    .selectize-dropdown { font-size:11px; max-height:220px; overflow-y:auto;}
    .selectize-input { font-size:12px; min-height:30px; padding:3px 8px;}
    /* zero out gap between auto and overall summaries */
    .card p.text-muted.small{ margin:2px 0 0 0 !important; }
    .card p.small{ margin:0 0 2px 0 !important; }
    /* grey-out look when disabled on Play */
    .greyed { pointer-events:none; opacity:0.45; }
  ")),

  sidebar = sidebar(
    width = 300,
    div(id = "filters_all",
        h4("Filters"),
        radioButtons("freq","Frequency", c("Monthly","Weekly","Daily"),
                     selected = "Monthly", inline = TRUE),
        selectInput("metric","Metric", c("Cases","Deaths","DALYs"), selected = "Cases"),
        checkboxGroupInput("hais","Infection types", choices = hai_levels,
                           selected = hai_levels, inline = FALSE),
        dateRangeInput("dates","Date range", start = date_min, end = date_max,
                       min = date_min, max = date_max),
        checkboxGroupInput("sexes","Sex", choices = sex_levels,
                           selected = sex_levels, inline = TRUE),
        #default facet for first two charts are Region × Sex
        selectInput("facet_by","Facet by",
                    choices = c("Region","Sex","Region × Sex","None"),
                    selected = "Region × Sex"),
        hr(),
        helpText("Tip: switch Frequency to change time granularity.")
    )
  ),

  nav(
    "Overview",
    card(
      full_screen = TRUE,
      card_header(textOutput("title_overview")),
      plotlyOutput("ts_violin", height = "520px"),
      card_body(
        p(class = "text-muted small", htmlOutput("ts_insight")),
        p(class = "small",          htmlOutput("ts_overall"))
      )
    )
  ),

  nav(
    "Heatmap",
    card(
      full_screen = TRUE,
      card_header(textOutput("title_heat")),
      plotlyOutput("heat", height = "560px"),
      card_body(
        p(class = "text-muted small", htmlOutput("heat_insight")),
        p(class = "small",          htmlOutput("heat_overall"))
      )
    )
  ),

  nav(
    "HAI Movement",
    layout_columns(
      col_widths = c(12),
      card(
        full_screen = TRUE,
        card_header(textOutput("title_play")),
        #month only slider
        sliderInput(
          "play_month", "Choose period",
          min = floor_date(date_min, "month"),
          max = floor_date(date_max, "month"),
          value = floor_date(date_max, "month"),
          step = 31,
          timeFormat = "%Y-%m",
          animate = animationOptions(interval = 600, loop = TRUE)
        ),
        plotlyOutput("play_ts", height = "520px"),
        card_body(
          p(class = "text-muted small", htmlOutput("play_insight")),
          p(class = "small",          htmlOutput("play_overall")),
          p(class = "text-muted small", "Note: This chart uses monthly data only for performance.")
        )
      )
    )
  )
)

#server
server <- function(input, output, session) {

  #grey out filters when on Play
  observe({
    if (identical(input$nav, "Play")) {
      addClass(selector = "#filters_all", class = "greyed")
    } else {
      removeClass(selector = "#filters_all", class = "greyed")
    }
  })

  #dynamic titles
  output$title_overview <- renderText({
    glue("Distribution of {input$metric} by HAI")
  })
  output$title_heat <- renderText({
    gran <- if (input$freq == "Weekly") "Week and HAI" else "Month and HAI"
    glue("Intensity across time by {gran}")
  })
  output$title_play <- renderText({
    glue("{input$metric} over time with moving HAI dots chart")
  })

  filtered <- reactive({
    req(input$hais, input$sexes, input$dates)
    agg_data(
      freq    = input$freq,
      metric  = input$metric,
      hais    = input$hais,
      sexes   = input$sexes,
      d_from  = input$dates[1],
      d_to    = input$dates[2]
    )
  })

  #overview (violin and jitter)
  output$ts_violin <- renderPlotly({
    df <- filtered()
    validate(need(nrow(df) > 0, "No data for the current filters."))

    p <- ggplot(df, aes(hai, value, fill = hai)) +
      geom_violin(alpha = 0.35, width = 0.9, color = NA, trim = FALSE) +
      geom_jitter(aes(color = hai), width = 0.12, height = 0, size = 1.3, alpha = 0.55) +
      scale_y_continuous(labels = label_comma()) +
      labs(x = "HAI", y = input$metric, fill = "HAI", color = "HAI") +
      theme_minimal(base_family = "Inter") +
      theme(legend.position = "none",
            strip.placement = "outside",
            strip.text = element_text(size = 10, face = "bold"),
            plot.margin = margin(6, 16, 6, 6))

    p <- facet_apply(p, input$facet_by)
    ggplotly(p, tooltip = c("x","y"))
  })
  output$ts_insight <- renderUI({ HTML(insight_text(filtered(), input$metric, input$facet_by)) })
  output$ts_overall <- renderUI({ HTML(overall_text(filtered(), input$metric)) })

  #heatmap
  output$heat <- renderPlotly({
    req(input$freq %in% c("Weekly","Monthly"))
    cols <- pick_metric_cols(input$metric)

    if (input$freq == "Weekly") {
      hm <- sim_weekly |>
        filter(hai %in% input$hais, sex %in% input$sexes,
               date >= input$dates[1], date <= input$dates[2]) |>
        mutate(week = isoweek(date), yr = year(date)) |>
        group_by(region, sex, hai, week) |>
        summarise(value = sum(.data[[cols[["week"]]]], na.rm = TRUE), .groups = "drop")
      xlab <- "ISO week"; xvar <- "week"
    } else {
      hm <- sim_monthly |>
        filter(hai %in% input$hais, sex %in% input$sexes,
               date >= input$dates[1], date <= input$dates[2]) |>
        mutate(month = month(date, label = TRUE, abbr = TRUE)) |>
        group_by(region, sex, hai, month) |>
        summarise(value = sum(.data[[cols[["month"]]]], na.rm = TRUE), .groups = "drop")
      xlab <- "Month"; xvar <- "month"
    }

    p <- ggplot(hm, aes(.data[[xvar]], hai, fill = value)) +
      geom_tile() +
      scale_fill_viridis_c(name = input$metric, labels = label_comma()) +
      labs(x = xlab, y = "") +
      theme_minimal(base_family = "Inter") +
      theme(legend.position = "right",
            strip.placement = "outside",
            strip.text = element_text(size = 10, face = "bold"),
            plot.margin = margin(6, 16, 6, 6))

    p <- facet_apply(p, input$facet_by)
    ggplotly(p, tooltip = c("x","y","fill"))
  })
  output$heat_insight <- renderUI({ HTML(insight_text(filtered(), input$metric, input$facet_by)) })
  output$heat_overall <- renderUI({ HTML(overall_text(filtered(), input$metric)) })

  #play tab: monthly only
  # Aggregate monthly once for current filters
  monthly_df <- reactive({
    cols <- pick_metric_cols(input$metric)
    sim_monthly |>
      filter(hai %in% input$hais, sex %in% input$sexes,
             date >= input$dates[1], date <= input$dates[2]) |>
      group_by(date, hai) |>
      summarise(value = sum(.data[[cols[["month"]]]], na.rm = TRUE), .groups = "drop")
  })

  #series and moving dots at the selected month
  output$play_ts <- renderPlotly({
    df_all <- monthly_df()
    validate(need(nrow(df_all) > 0, "No data for the selected period."))

    current_m <- floor_date(input$play_month, "month")
    df_now <- df_all |> filter(floor_date(date, "month") == current_m)

    p <- ggplot(df_all, aes(date, value, colour = hai, group = hai)) +
      geom_line(linewidth = 0.7) +
      geom_point(data = df_now, size = 3.2) +  #moving dots
      scale_x_date(date_labels = "%b %Y") +
      scale_y_continuous(labels = label_comma()) +
      labs(x = "Month–Year", y = input$metric, colour = "HAI") +
      theme_minimal(base_family = "Inter") +
      theme(legend.position = "bottom",
            plot.margin = margin(6, 16, 6, 6))

    #no facet on Play
    ggplotly(p, tooltip = c("x","y","colour")) |>
      layout(legend = list(orientation = "h", x = 0, y = -0.2))
  })

  output$play_insight <- renderUI({ HTML(insight_text(monthly_df(), input$metric, "None")) })
  output$play_overall <- renderUI({ HTML(overall_text(monthly_df(), input$metric)) })
}

shinyApp(ui, server)
