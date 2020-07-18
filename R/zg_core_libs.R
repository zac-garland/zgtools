#' zg_core_libs
#' loads favorite functions
#'
#' @param core condensed packages = T all favs = F
#' @param highlight highlights all function names within code in Rmd
#' @export
#' @examples
#' zg_core_libs(core = T, highlight = F)

zg_core_libs <- function(core = T, highlight = F) {
  if (core) {
    if (highlight) {
      highlight_load(
        here, htmlwidgets,
        janitor, lubridate,
        kableExtra, pacman,
        plotly, readxl,
        rvest, shiny,
        tidyquant, tidyverse
      )
    } else {
      pacman::p_load(
        here, htmlwidgets,
        janitor, lubridate,
        kableExtra, pacman,
        plotly, readxl,
        rvest, shiny,
        tidyquant, tidyverse
      )
    }
  } else {
    if (highlight) {
      highlight_load(
        clipr, flexdashboard,
        here, highcharter,
        htmlwidgets, janitor,
        lubridate, kableExtra,
        pacman, plotly,
        readxl, rhandsontable,
        rvest, shiny,
        shinycssloaders, shinydashboard,
        shinyWidgets, shinybusy,
        styler, tidyquant,
        tidyverse
      )
    } else {
      pacman::p_load(
        clipr, flexdashboard,
        here, highcharter,
        htmlwidgets, janitor,
        lubridate, kableExtra,
        pacman, plotly,
        readxl, rhandsontable,
        rvest, shiny,
        shinycssloaders, shinydashboard,
        shinyWidgets, shinybusy,
        styler, tidyquant,
        tidyverse
      )
    }
  }
}


#' @importFrom magrittr %>%
