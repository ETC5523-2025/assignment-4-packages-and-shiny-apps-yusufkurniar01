#data-raw/simulate_hai_germany.R
#purpose: read the 2011 PPS snapshot, simulate monthly/weekly/daily series
#for 2011–2012

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(usethis)
  library(janitor)
  library(purrr)
})

set.seed(35425067)

rng_start <- as.Date("2011-01-01")
rng_end   <- as.Date("2012-12-31")

#source path
src_csv <- file.path("inst", "extdata", "hai_germany_2011.csv")
if (!file.exists(src_csv)) stop("Source CSV not found at inst/extdata/hai_germany_2011.csv")

#read and normalise names
raw <- readr::read_csv(src_csv, show_col_types = FALSE) |> clean_names()

#check required inputs
required_cols <- c("infection_type", "cases", "deaths", "dalys")
missing_cols  <- setdiff(required_cols, names(raw))
if (length(missing_cols)) {
  stop("Missing required columns in source CSV: ", paste(missing_cols, collapse = ", "))
}

#keep only the relevant variables
base <- raw |>
  filter(!str_to_lower(infection_type) %in% c("all", "total")) |>
  transmute(
    hai          = as.character(infection_type),
    cases_total  = as.numeric(cases),
    deaths_total = as.numeric(deaths),
    dalys_total  = as.numeric(dalys)
  )

if (nrow(base) == 0) stop("No usable rows after cleaning base data.")

#helper for integer >= 0
int_pos <- function(x) pmax(0L, as.integer(round(x)))

#simulate MONTHLY (2011-01 to 2012-12)
dates_m <- seq(as.Date("2011-01-01"), as.Date("2012-12-01"), by = "month")

sim_monthly <- tidyr::expand_grid(date = dates_m, hai = unique(base$hai)) |>
  left_join(base, by = "hai") |>
  mutate(
    year  = year(date),
    month = month(date),
    #small noise and mild winter seasonality
    monthly_factor = runif(n(), 0.85, 1.15),
    seasonal_adj   = 1 + 0.10 * sin(2 * pi * (month - 1) / 12),

    cases_month   = (cases_total  / 12) * monthly_factor * seasonal_adj,
    deaths_month  = (deaths_total / 12) * monthly_factor,
    dalys_month   = (dalys_total  / 12) * monthly_factor,

    cases_month   = int_pos(cases_month),
    deaths_month  = int_pos(deaths_month),
    dalys_month   = round(dalys_month, 1)
  ) |>
  select(date, year, month, hai, cases_month, deaths_month, dalys_month)

#simulate WEEKLY by splitting each month into ISO weeks
month_weeks <- function(date) {
  d1 <- as.Date(cut(date, "month")) #first day of month
  d2 <- as.Date(cut(date + months(1), "month")) - 1 #last day of month

  #first Monday ON or after day 1, and last Monday ON or BEFORE d2
  w1 <- lubridate::ceiling_date(d1, "week", week_start = 1)
  wN <- lubridate::floor_date(  d2, "week", week_start = 1)

  out <- if (w1 <= wN) seq(w1, wN, by = "1 week") else as.Date(character())
  #clamp to global range
  out[out >= rng_start & out <= rng_end]
}

split_to_weeks <- sim_monthly |>
  group_by(year, month, hai) |>
  reframe(
    week_start = month_weeks(as.Date(sprintf("%04d-%02d-01", unique(year), unique(month)))),
    n_weeks    = length(week_start)
  ) |>
  ungroup()

week_profile <- function(n) {
  n <- as.integer(n)
  if (is.na(n) || n < 1) n <- 1L
  p <- rep(1, n)
  if (n >= 2) { p[1]  <- 0.9; p[n]   <- 0.9 }
  if (n >= 3) { p[2]  <- 1.05; p[n-1] <- 1.05 }
  p / sum(p)
}

sim_weekly <- sim_monthly |>
  left_join(split_to_weeks, by = c("year","month","hai")) |>
  group_by(year, month, hai) |>
  arrange(week_start, .by_group = TRUE) |>
  mutate(
    week_idx = dplyr::row_number(),
    w = { n_w <- dplyr::n(); week_profile(n_w)[week_idx] }
  ) |>
  ungroup() |>
  transmute(
    date = week_start,
    year = lubridate::year(date),
    iso_week = isoweek(date),
    hai,
    cases_week = int_pos(cases_month * w),
    deaths_week = int_pos(deaths_month * w),
    dalys_week = round(dalys_month * w, 1)
  ) |>
  arrange(date, hai) |>
  dplyr::filter(date >= rng_start, date <= rng_end)

#simulate DAILY by splitting each week across 7 days
day_profile <- c(1.00, 1.00, 1.02, 1.03, 1.05, 0.95, 0.95)
day_profile <- day_profile / sum(day_profile)

week_days <- function(week_start) seq(week_start, by = "1 day", length.out = 7)

sim_daily <- sim_weekly |>
  mutate(day = map(date, week_days), w7 = list(day_profile)) |>
  tidyr::unnest(c(day, w7)) |>
  group_by(date, hai) |>
  mutate(
    cases_day  = int_pos(cases_week * w7),
    deaths_day = int_pos(deaths_week * w7),
    dalys_day  = round(dalys_week * w7, 2)
  ) |>
  ungroup() |>
  transmute(
    date = day,
    year = year(date),
    yday = yday(date),
    wday = wday(date, week_start = 1),
    hai, cases_day, deaths_day, dalys_day
  ) |>
  arrange(date, hai) |>
  dplyr::filter(date >= rng_start, date <= rng_end)

#write CSVs for inspection
readr::write_csv(sim_monthly, "inst/extdata/hai_germany_sim_monthly_2011_2012.csv")
readr::write_csv(sim_weekly,  "inst/extdata/hai_germany_sim_weekly_2011_2012.csv")
readr::write_csv(sim_daily,   "inst/extdata/hai_germany_sim_daily_2011_2012.csv")

#save binary data for the package
usethis::use_data(sim_monthly, sim_weekly, sim_daily, overwrite = TRUE)

message("Done. Created:",
        "\n- data/sim_monthly.rda",
        "\n- data/sim_weekly.rda",
        "\n- data/sim_daily.rda",
        "\n- CSVs in inst/extdata/")
