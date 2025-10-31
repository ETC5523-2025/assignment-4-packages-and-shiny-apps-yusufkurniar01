#' Launch the Shiny app
#'
#' Starts the Germany HAI Explorer included with this package.
#' @return No return value, and called for side effects.
#' @export
launch_app <- function() {
  # Resolve the app directory inside inst/
  app_dir <- system.file("app", package = "yusufHAIGermany")
  if (app_dir == "") {
    stop("App directory not found. Build/install the package, then try again.", call. = FALSE)
  }

  # Run the Shiny app normally
  shiny::runApp(app_dir, display.mode = "normal")
}

