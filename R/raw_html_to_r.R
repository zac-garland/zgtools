#' converts raw html to an r function
#'
#' @param raw_html character vector of html text
#' @param length_of_param length of original input to use as placeholder
#' @return html function template copied to clipboard
#' @seealso <links to other help pages>
#' @export
#' @examples
#' raw_html_to_r('<div class="card" style="max-width: 18rem;"><p style="color-red;">text</p></div>', length_of_param = -1)



raw_html_to_r <- function(raw_html, length_of_param = 30) {
  raw_html <- c("<div>", as.character(raw_html), "</div>")

  to_build <- raw_html %>%
    stringr::str_replace_all("\\n", "") %>%
    stringr::str_split(">") %>%
    unlist() %>%
    stringr::str_trim() %>%
    stringr::str_squish() %>%
    dplyr::tibble(line = .) %>%
    dplyr::filter(line != "") %>%
    dplyr::mutate(line = paste0(line, " >")) %>%
    dplyr::mutate(line = stringr::str_replace_all(line, stringr::regex("</[a-zA-Z0-9]+ >"), ")")) %>%
    dplyr::mutate(line = dplyr::case_when(
      stringr::str_detect(line, "<") ~ stringr::str_replace(line, "<", "") %>%
        stringr::str_replace(" ", "(") %>%
        stringr::str_trim() %>%
        stringr::str_squish(),
      T ~ line
    )) %>%
    dplyr::mutate(line = stringr::str_replace(line, " >", ">")) %>%
    dplyr::mutate(line = dplyr::case_when(
      stringr::str_count(line, " ") > 0 ~ stringr::str_replace_all(line, '" ', '", ') %>%
        stringr::str_replace_all(">", ","),
      T ~ line
    )) %>%
    dplyr::mutate(line = dplyr::case_when(
      stringr::str_sub(line, -2, -1) == "(>" ~ stringr::str_replace_all(line, ">", ""),
      T ~ stringr::str_replace_all(line, ">", ",")
    )) %>%
    dplyr::mutate(function_arg = dplyr::case_when(
      (str_detect(line, "\\)") & stringr::str_length(line) > 1) ~ paste0(str_sub(snakecase::to_snake_case(line), 1, 20)),
      T ~ NA_character_
    )) %>%
    dplyr::group_by(function_arg) %>%
    dplyr::mutate(function_arg = ifelse(!is.na(function_arg), paste0(function_arg, "_", dplyr::row_number()), NA_character_)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(function_examp = dplyr::case_when(
      !is.na(function_arg) ~ paste(function_arg, "=", shQuote(str_sub(str_replace(line, "\\)", ""), 1, length_of_param)))
    )) %>%
    dplyr::mutate(line = dplyr::case_when(
      !is.na(function_arg) ~ paste0(function_arg, ")"),
      T ~ line
    ))

  fun_body <- to_build %>%
    dplyr::mutate(line = dplyr::case_when(
      stringr::str_detect(line, "\\)") & dplyr::lead(line) != ")" ~ paste0(line, ","),
      T ~ line
    )) %>%
    dplyr::pull(line) %>%
    paste(collapse = " ") %>%
    stringr::str_replace_all("\\(", "\\(\n") %>%
    stringr::str_replace_all("\\)", "\n\\)") %>%
    stringr::str_replace_all(",", ",\n")

  fun_args <- to_build %>%
    stats::na.omit() %>%
    dplyr::pull(function_examp) %>%
    paste(collapse = ", ") %>%
    stringr::str_replace_all(",", ",\n")



  fun_build <- paste(
    "new_fun <- function(", fun_args, "){", fun_body, "}"
  ) %>%
    styler::style_text()


  clipr::write_clip(fun_build)

  cat(fun_build,sep="\n")
  message(paste("html template copied to clipboard"))


}
