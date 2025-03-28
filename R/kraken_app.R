#' @export
kraken_app <- function() {
  appDir <- system.file("shinyapp", "kraken", package = "kraken")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
