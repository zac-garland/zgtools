zg_app <- function(app_name) {

  valid_apps <- list.files(system.file("shiny-apps", package = "zgtools"))

  # if an invalid app_name is given, throw an error
  if (missing(app_name) || !nzchar(app_name) ||
      !app_name %in% valid_apps) {

    valid_apps

  } else{
    appDir <- system.file("shiny-apps", app_name, package = "zgtools")
    shiny::runApp(appDir, display.mode = "normal")


  }

}
