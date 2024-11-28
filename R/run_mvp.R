#' @name run_mvp
#'
#' @title
#' Launch Model Visualization Platform App
#'
#' @description
#' This function will launch MVP app and pass along arbitrary parameters through
#' the \code{...} parameter to the application. This is done by modifying the
#' global environment. This function will attempt to clean up any objects placed
#' into the global environment on exit. If objects exist prior to calling this
#' function (i.e. \code{exists(OBJECT)} returns TRUE) then the value will be reset
#' to it's state prior to calling \code{run_mvp}.
#'
#' @details
#' The settings below are not technically arguments,
#' however when provided will toggle the app behavior:
#' * `show_debugging_msg` Logical. Default `FALSE`. Set to `TRUE` to output verbose
#'  working messages in the console, could be useful for debugging.
#' * `authentication_code` Default `NULL`. Provide a string (i.e. password) to
#'  password-lock the entire app.
#' * `insert_watermark` Logical. Default `TRUE`. Set to `FALSE` to remove
#' "For Internal Use Only" text in simulated plots.
#' * `internal_version` Logical. Default `TRUE`. Setting to `FALSE` may allow
#' generation of NCA reports when hosted on AWS with different access rights.
#' * `use_bi_styling` Logical. Default `FALSE`. Set to TRUE to insert BI logo.
#' * `pw_models_path` Default `NULL`. provide a path to source in password-gated
#' models (see the `shiny/passworded_models_example.R` for ideas, and the
#' vignette: \href{../doc/supply-passwords.html}{supply passworded models})
#'
#' Note that these variables are provided internally by default and does not exist
#' as an object - see examples below for its correct usage.
#'
#' If the user wishes to run the App outside of the function (e.g. preparing for
#' deployment on Posit Connect), this can be done by accessing inst/shiny/app.R,
#' which is located inside the folder of where the package was installed, and set
#' debug_mode = TRUE (and settings these options there as required).
#'
#' @param appDir the directory of the application to run.
## @param ui the Shiny ui object.
## @param server the Shiny server object.
#' @param ... [shiny::runApp()] parameters, [shiny::shinyApp()] parameters,
#'        or parameters to pass to the Shiny app.
#' @examples
#' \dontrun{
#' run_mvp(insert_watermark = FALSE) # remove watermarks
#' run_mvp(authentication_code = "some_password") # Password-lock the site,
#' # could be useful in deployment
#' run_mvp(pw_models_path = "path/to/your/private/models.R") # see
#' # "passworded_models_example.R" on how to set one up
#' }
#' @note
#' Adapted from https://github.com/jbryer/ShinyDemo/blob/master/R/run_shiny_app.R
#' @seealso
#' \code{vignette("supply-passwords", package = "MVPapp")}
#' @export
run_mvp <- function(appDir = system.file("shiny", package = "MVPapp"),
                    ...) {

  params <- list(...)
  shinyApp_args <- list()
  runApp_args <- list()

  if(length(params) > 0) {
    # If any parameter in ... exists elsewhere, save the original so
    # we can reset it on.exit.
    reset_params <- list()

    runApp_params <- names(formals(shiny::runApp))
    shinyApp_params <- names(formals(shiny::shinyApp))

    runApp_args <- params[names(params) %in% runApp_params]
    shinyApp_args <- params[names(params) %in% shinyApp_params]
    app_args <- params[!names(params) %in% c(runApp_params, shinyApp_params)]

    for(i in names(app_args)) {
      # if(exists(i, envir = parent.env(environment()))) {
      # This seems to work better since exists(mtcars), for example, will
      # always return TRUE and therefore would be left in the environment.
      # This lists all objects available in the call stack.
      if(i %in% ls_all()) {
        reset_params[[i]] <- get(i)
      }
      .GlobalEnv[[i]] <- app_args[[i]]
    }

    if(length(app_args) > 0) {
      on.exit({
        # Clean up the environment to leave it the way it was before.
        rm(list = names(app_args), envir = .GlobalEnv)
        for(i in names(reset_params)) {
          .GlobalEnv[[i]] <- reset_params[[i]]
        }
      })
    }
  }

  # Loading packages here to side-step JS compatibility issues
  source(file = file.path(appDir, "packages.R"))

  default_options <- options()
  options(scipen=3) # Set the penalty to a high value to avoid scientific notation, this value is good up until 3e-07 / 1e+08
  options(DT.options = list(pageLength = 20, language = list(search = 'Filter:'), scrollX = T)) # dataTable options
  options(shiny.maxRequestSize = 100*1024^2) # Maximum file upload size

  runApp_args$appDir <- appDir
  do.call(shiny::runApp, runApp_args)
}

#' List All Objects
#'
#' This function returns a character vector of all objects available. Unlike
#' [ls()] this function will loop through all environments from the current
#' environment to \code{.GlobalEnv}. This will also verify that the object
#' is indeed available from the current environment using the [exists()]
#' function call.
#'
#' @return a character vector with the name of all objects available.
#' @note
#' Copied from https://github.com/jbryer/ShinyDemo, R/ls_all.R
ls_all <- function() {
  objs <- character()
  i <- 1
  repeat {
    local_objs <- ls(parent.frame(i))
    # Confirm the object is available from the current environment
    for(j in local_objs) {
      if(exists(j)) {
        objs <- c(objs, j)
      }
    }
    if(identical(parent.frame(i), .GlobalEnv)) {
      break
    } else {
      i <- i + 1
    }
  }
  return(objs)
}

