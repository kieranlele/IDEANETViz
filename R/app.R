#' Launch Shiny App
#'
#' @param name The name of the app to run
#' @param ... arguments to pass to shiny::runApp
#'
#' @export
#'
ideanetViz <- function(name = "ideanetViz", ...) {
  appDir <- system.file(paste0("apps/", name), package = "ideanetViz")
  if (appDir == "") stop("The shiny app ", name, " does not exist")
  shiny::runApp(appDir, display.mode = 'showcase', ...)
}