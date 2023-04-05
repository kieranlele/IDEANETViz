#' Launch Shiny App
#'
#' @param name The name of the app to run
#' @param ... arguments to pass to shiny::runApp
#'
#' @export
#'
ideanet_viz <- function(name = "ideanet_viz", ...) {
  appDir <- system.file(paste0("apps/", name), package = "IDEANETViz")
  if (appDir == "") stop("The shiny app ", name, " does not exist")
  shiny::runApp(appDir, ...)
}