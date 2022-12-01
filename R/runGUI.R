################################################################################
#
# Running results dashboard
#
################################################################################


#' This function starts the dashboard application for comparing results from different databases
#'
#' @param pathToResults Path to target directory where results will be saved
#' @export
runDashboard <- function(pathToResults = NULL) {
  runDashboard <<- pathToResults
  shiny::runApp("./resultsDashboard")
}
