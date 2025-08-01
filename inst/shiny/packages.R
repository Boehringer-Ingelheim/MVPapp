#-------------------------------------------------------------------------------
#' @name check_and_install_packages
#' @title Check and Install Required Packages
#'
#' @description This function checks whether the required packages for a Shiny 
#' application are installed and meet the specified minimum version requirements. 
#' If any packages are missing or outdated, the user is prompted to install or 
#' update them. If the user declines, the function throws an error and stops execution.
#'
#' @param required_packages A named list where the names are package names and the values are the minimum required versions (as character strings). 
#' For example: `list(package_name = "version")`.
#'
#' @return This function does not return a value. 
#' 
#' @details It ensures that all required packages are installed and up-to-date.
#' If the user declines to install/update missing or outdated packages, the function
#' stops execution with an error message.
#'
#' @examples
#' \dontrun{
#' required_packages <- list(
#'   shiny = "1.7.4.1",
#'   ggplot2 = "3.4.4",
#'   dplyr = "0.0.0"
#' )
#' 
#' check_and_install_packages(required_packages)
#' }
#'
#' @export
#-------------------------------------------------------------------------------
check_and_install_packages <- function(required_packages) {
  # Check for missing or outdated packages
  missing_or_outdated <- lapply(names(required_packages), function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      return(pkg)  # Package is missing
    } else {
      installed_version <- packageVersion(pkg)
      required_version <- required_packages[[pkg]]
      if (installed_version < required_version) {
        return(pkg)  # Package is outdated
      }
    }
    return(NULL)  # Package is up-to-date
  })
  
  # Filter out NULL values (packages that are up-to-date)
  missing_or_outdated <- unlist(missing_or_outdated)
  
  # If there are missing or outdated packages
  if (length(missing_or_outdated) > 0) {
    message("The following packages are missing or outdated: ", 
            paste(missing_or_outdated, collapse = ", "))
    response <- readline(prompt = "Would you like to install/update them? (y/n): ")
    
    if (tolower(response) == "y") {
      install.packages(missing_or_outdated)
    } else {
      stop("Missing or outdated required packages. Cannot proceed.")
    }
  }
}

required_packages <- list(
  bslib = "0.5.1",
  cowplot = "0.0.0", 
  data.table = "1.14.2", 
  dplyr = "0.0.0",
  DT = "0.0.0", 
  flextable = "0.6.10",
  forcats = "0.0.0",
  GGally = "0.0.0",
  ggplot2 = "3.4.4",
  grid = "0.0.0", 
  gridExtra = "0.0.0", 
  htmltools = "0.5.5",
  magrittr = "0.0.0",
  mrgsolve = "1.5.0",
  plotly = "4.10.1", 
  purrr = "0.0.0",
  rhandsontable = "0.3.8",
  rlang = "0.0.0", 
  scales = "0.0.0",
  shiny = "1.7.4.1",
  shinyAce = "0.0.0", 
  shinyBS = "0.61.1", 
  shinycssloaders = "0.0.0",
  shinydashboard = "0.7.2",
  shinyjs = "1.0", 
  shinythemes = "0.0.0",
  shinyWidgets = "0.8.0",
  stringr = "0.0.0", 
  tibble = "0.0.0",
  tidyr = "0.0.0"
)

check_and_install_packages(required_packages)