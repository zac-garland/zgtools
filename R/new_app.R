#' creates a boiler plate app in the proper folder under inst
#'
#' @param app_name name of new app
#' @param remove_comments logical remove app template comments
#' @export
#' @examples
#' new_app(app_name = "test", remove_comments = TRUE)



new_app <- function(app_name = NULL,remove_comments = TRUE){

    c("inst","inst/shiny-apps") %>%
    purrr::map(~{
      if(!dir.exists(.x)) dir.create(.x)
    }) %>%
    invisible()

  app_name <- snakecase::to_snake_case(app_name)

  dir_to_create <- file.path("inst/shiny-apps",app_name)

  dir.create(dir_to_create)

  to_copy <- system.file("examples","01_hello","app.R",package = "shiny")

  file.copy(to_copy,dir_to_create)

  new_app_path <- file.path(dir_to_create,"app.R")

  if(remove_comments){
    readr::read_lines(new_app_path) %>%
      dplyr::tibble(line = .) %>%
      dplyr::mutate(check = stringr::str_trim(line) %>% stringr::str_squish()) %>%
      dplyr::filter(stringr::str_sub(check,1,1) != "#") %>%
      dplyr::pull(line) %>%
      styler::style_text() %>%
      readr::write_lines(new_app_path)
  }else {
    readr::read_lines(new_app_path) %>%
      dplyr::tibble(line = .) %>%
      dplyr::pull(line) %>%
      styler::style_text() %>%
      readr::write_lines(new_app_path)

  }




  file.edit(new_app_path)


}
