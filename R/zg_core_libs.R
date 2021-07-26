.onAttach <- function(...){
  zg_attach()
}


core <- c("plotly","htmltools","janitor","lubridate","readxl","rvest","shiny","tidyquant","tidyverse","highcharter","reactable","dplyr","tidyr")



core_unloaded <- function(){
  search <- paste0("package:",core)
  core[!search %in% search()]
}

same_library <- function(pkg){
  loc <- if (pkg %in% loadedNamespaces()) dirname(getNamespaceInfo(pkg,"path"))

  do.call(
    "library",
    list(pkg, lib.loc = loc, character.only = TRUE, warn.conflicts = FALSE)
  )

}


zg_attach <- function(){
  to_load <- core_unloaded()
  if(length(to_load)==0)
    return(invisible())

  suppressPackageStartupMessages(
    lapply(to_load,same_library)
  )

  invisible()
}


#' @importFrom magrittr %>%

#' @importFrom rstudioapi restartSession
