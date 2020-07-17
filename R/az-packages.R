#' @importFrom magrittr %>%

.onAttach <- function(...) {
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(
    clipr,
    flexdashboard,
    here,
    highcharter,
    htmlwidgets,
    janitor,
    lubridate,
    kableExtra,
    pacman,
    plotly,
    readxl,
    rhandsontable,
    rvest,
    shiny,
    shinycssloaders,
    shinydashboard,
    shinyWidgets,
    shinybusy,
    styler,
    tidyquant,
    tidyverse
  )
}
