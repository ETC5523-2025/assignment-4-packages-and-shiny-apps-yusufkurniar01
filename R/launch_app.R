#' Launch the Shiny app
#'
#' Starts the Germany HAI Explorer included with this package.
#' @return No return value, and called for side effects.
#' @export
launch_app <- function() {
  #resolve the app directory inside inst/app
  app_dir <- system.file("app", package = "yusufHAIGermany")
  if (app_dir == "" || !dir.exists(app_dir)) {
    stop("App directory not found. Build/install the package, then try again.",
         call. = FALSE)
  }

  #run the Shiny app
  shiny::shinyAppDir(app_dir, options = list(launch.browser = TRUE))
}


