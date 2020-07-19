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
    str_replace_all("\\n", "") %>%
    str_split(">") %>%
    unlist() %>%
    str_trim() %>%
    str_squish() %>%
    tibble(line = .) %>%
    filter(line != "") %>%
    mutate(line = paste0(line, " >")) %>%
    mutate(line = str_replace_all(line, regex("</[a-zA-Z0-9]+ >"), ")")) %>%
    mutate(line = case_when(
      str_detect(line, "<") ~ str_replace(line, "<", "") %>%
        str_replace(" ", "(") %>%
        str_trim() %>%
        str_squish(),
      T ~ line
    )) %>%
    mutate(line = str_replace(line, " >", ">")) %>%
    mutate(line = case_when(
      str_count(line, " ") > 0 ~ str_replace_all(line, '" ', '", ') %>%
        str_replace_all(">", ","),
      T ~ line
    )) %>%
    mutate(line = case_when(
      str_sub(line, -2, -1) == "(>" ~ str_replace_all(line, ">", ""),
      T ~ str_replace_all(line, ">", ",")
    )) %>%
    mutate(function_arg = case_when(
      (str_detect(line, "\\)") & str_length(line) > 1) ~ paste0(str_sub(snakecase::to_snake_case(line), 1, 20)),
      T ~ NA_character_
    )) %>%
    group_by(function_arg) %>%
    mutate(function_arg = ifelse(!is.na(function_arg), paste0(function_arg, "_", row_number()), NA_character_)) %>%
    ungroup() %>%
    mutate(function_examp = case_when(
      !is.na(function_arg) ~ paste(function_arg, "=", shQuote(str_sub(str_replace(line, "\\)", ""), 1, length_of_param)))
    )) %>%
    mutate(line = case_when(
      !is.na(function_arg) ~ paste0(function_arg, ")"),
      T ~ line
    ))

  fun_body <- to_build %>%
    mutate(line = case_when(
      str_detect(line, "\\)") & lead(line) != ")" ~ paste0(line, ","),
      T ~ line
    )) %>%
    pull(line) %>%
    paste(collapse = " ") %>%
    str_replace_all("\\(", "\\(\n") %>%
    str_replace_all("\\)", "\n\\)") %>%
    str_replace_all(",", ",\n")

  fun_args <- to_build %>%
    na.omit() %>%
    pull(function_examp) %>%
    paste(collapse = ", ") %>%
    str_replace_all(",", ",\n")



  fun_build <- paste(
    "new_fun <- function(", fun_args, "){", fun_body, "}"
  ) %>%
    styler::style_text()


  clipr::write_clip(fun_build)

  cat(fun_build,sep="\n")
  message(paste("html template copied to clipboard"))


}
