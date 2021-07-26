#' create an rmarkdown document based on downcute chaos theme
#'
#' @param report_name name of report
#' @export
#' @examples
#' new_chaos_report(report_name = "test")



new_chaos_report <- function(report_name,...){
  report_name = snakecase::to_snake_case(report_name) %>%
    stringr::str_replace_all("_", "-") %>%
    paste0(".Rmd")

  usethis::use_directory("inst")

  rmarkdown::draft(
    file = glue::glue("inst/{report_name}"),
    template = "downcute_chaos",
    package = "rmdformats",
    create_dir = TRUE,
    edit = FALSE
  )


  new_rmd_path <-
    glue::glue(
      "inst/{stringr::str_replace(basename(report_name),paste0('.',tools::file_ext(report_name)),'')}/{report_name}"
    )

  file.edit(new_rmd_path)
}
