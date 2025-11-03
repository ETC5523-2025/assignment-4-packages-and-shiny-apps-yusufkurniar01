#' Simulated German HAI monthly series, 2011–2012
#'
#' Synthetic monthly counts and DALYs by HAI type (HAP, SSI, BSI, UTI, CDI).
#'
#' @format A tibble with columns:
#' \describe{
#'   \item{date}{Date (first day of month)}
#'   \item{year}{Integer year}
#'   \item{month}{Integer month}
#'   \item{hai}{Character infection type}
#'   \item{cases_month}{Integer simulated cases}
#'   \item{deaths_month}{Integer simulated deaths}
#'   \item{dalys_month}{Numeric simulated DALYs}
#' }
#' @keywords datasets
"sim_monthly"

#' Simulated German HAI weekly series, 2011–2012
#'
#' @format A tibble with columns:
#' \describe{
#'   \item{date}{Date for ISO week start (Monday)}
#'   \item{year}{Integer year}
#'   \item{iso_week}{Integer ISO week number}
#'   \item{hai}{Character infection type}
#'   \item{cases_week}{Integer simulated cases}
#'   \item{deaths_week}{Integer simulated deaths}
#'   \item{dalys_week}{Numeric simulated DALYs}
#' }
#' @keywords datasets
"sim_weekly"

#' Simulated German HAI daily series, 2011–2012
#'
#' @format A tibble with columns:
#' \describe{
#'   \item{date}{Date}
#'   \item{year}{Integer year}
#'   \item{yday}{Day-of-year (1–366)}
#'   \item{wday}{Day-of-week (1=Mon, 7=Sun)}
#'   \item{hai}{Character infection type}
#'   \item{cases_day}{Integer simulated cases}
#'   \item{deaths_day}{Integer simulated deaths}
#'   \item{dalys_day}{Numeric simulated DALYs}
#' }
#' @keywords datasets
"sim_daily"
