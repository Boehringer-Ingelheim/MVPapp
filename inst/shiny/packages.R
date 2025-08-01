required_packages <- list(
  bslib = "0.5.1",
  cowplot = NULL, 
  data.table = "1.14.2", 
  dplyr = NULL,
  DT = NULL, 
  flextable = "0.6.10",
  forcats = NULL,
  GGally = NULL,
  ggplot2 = "3.4.4",
  grid = NULL, 
  gridExtra = NULL, 
  htmltools = "0.5.5",
  magrittr = NULL,
  mrgsolve = "1.5.0",
  plotly = "4.10.1", 
  purrr = NULL,
  rhandsontable = "0.3.8",
  rlang = NULL, 
  scales = NULL,
  shiny = "1.7.4.1",
  shinyAce = NULL, 
  shinyBS = "0.61.1", 
  shinycssloaders = NULL,
  shinydashboard = "0.7.2",
  shinyjs = "1.0", 
  shinythemes = NULL,
  shinyWidgets = "0.8.0",
  stringr = NULL, 
  tibble = NULL,
  tidyr = NULL
)

# Find missing or outdated packages
packages_to_update <- character(0)

for (pkg in names(required_packages)) {
  min_ver <- required_packages[[pkg]]
  
  # Check if package is installed
  if (!requireNamespace(pkg, quietly = TRUE)) {
    packages_to_update <- c(packages_to_update, pkg)
  } else if (!is.null(min_ver)) {
    current_ver <- as.character(packageVersion(pkg))
    if (compareVersion(current_ver, min_ver) < 0) {
      packages_to_update <- c(packages_to_update, pkg)
    }
  }
}

# If not all packages are up to date
if (length(packages_to_update) > 0) {
  cat("The following packages are missing or outdated:\n")
  print(packages_to_update)
  update_choice <- readline(prompt = "Would you like to install/update them? [yes/no]: ")
  
  if (tolower(update_choice) != "yes") {
    cat("Update process canceled by user. App not started.\n")
    quit(save = "no")
  }
  
  install.packages(packages_to_update, dependencies = TRUE)
}

