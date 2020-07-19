#' converts raw html to an r function using withTags
#'
#' @param raw_html character vector of html text
#' @param length_of_param length of original input to use as placeholder
#' @return html function template copied to clipboard
#' @export
#' @examples
#' raw_html_to_r('<div class="card" style="max-width: 18rem;"><p style="color-red;">text</p></div>', length_of_param = -1)



raw_html_to_r <- function(raw_html, length_of_param = 30) {


  to_build <- preprocess_html(raw_html,length_of_param)

  length_el <- xml2::read_html(as.character(paste(raw_html,collapse=""))) %>%
    rvest::html_nodes("head,body,footer") %>%
    length()

  if(length_el>1)
    enclosing_el <- "html"
  else
    enclosing_el <- "div"


  fun_body <- to_build %>%
    dplyr::mutate(line = dplyr::case_when(
      stringr::str_detect(line, "\\)") & dplyr::lead(line) != ")" ~ paste0(line, ","),
      T ~ line
    )) %>%
    mutate(line = case_when(
      str_sub(line,-1,-1) == "," & str_sub(lead(line),1,1) == ")" ~ stringi::stri_reverse(line) %>%
        str_replace(",","") %>% stringi::stri_reverse(.),
      T ~ line
    )) %>%
    dplyr::pull(line) %>%
    paste(collapse = " ") %>%
    paste0("shiny::withTags(",enclosing_el,"(",.,"))") %>%
    stringr::str_replace_all("\\(", "\\(\n") %>%
    stringr::str_replace_all("\\)", "\n\\)") %>%
    stringr::str_replace_all(",", ",\n")


  fun_args <- to_build %>%
    filter(!is.na(function_examp)) %>%
    dplyr::pull(function_examp) %>%
    paste(collapse = ", ") %>%
    stringr::str_replace_all(",", ",\n") %>%
    paste(", ...")



  fun_build <- paste(
    "new_fun <- function(", fun_args, "){", fun_body, "}"
  ) %>%
    styler::style_text()


  clipr::write_clip(fun_build)

  cat(fun_build,sep="\n")
  message(paste("html template copied to clipboard"))


}



preprocess_html <- function(raw_html, length_of_param = 30) {
  initial_process <-
    xml2::read_html(as.character(paste(raw_html,collapse=""))) %>%
    rvest::html_nodes("head,body,footer") %>%
    as.character() %>%
    stringr::str_replace_all(regex("<!--(.*?)-->"), "") %>%
    stringr::str_replace_all("\\n", "") %>%
    stringr::str_split(">") %>%
    unlist() %>%
    stringr::str_trim() %>%
    stringr::str_squish() %>%
    dplyr::tibble(line = .) %>%
    dplyr::filter(line != "") %>%
    dplyr::mutate(line = paste0(line, " >")) %>%
    tibble::rowid_to_column()

  # classify
  self_enclosing <- initial_process %>%
    dplyr::filter(str_detect(line, "<|>")) %>%
    tidyr::separate(line, c("other", "tag"), sep = "\\<") %>%
    tidyr::separate(tag, c("tag", "other"), sep = " ") %>%
    dplyr::mutate(self_enclosing = dplyr::case_when(str_detect(tag, "\\/") ~ T, T ~ NA)) %>%
    dplyr::mutate(tag = stringr::str_replace_all(tag, "\\/", "")) %>%
    dplyr::group_by(tag) %>%
    dplyr::mutate(self_enclosing = sum(self_enclosing, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(self_enclosing == 0) %>%
    dplyr::pull(rowid)



 initial_process %>%
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
      stringr::str_detect(lead(line), "\\(") ~ stringr::str_replace_all(line, ">", ","),
      T ~ stringr::str_replace_all(line, ">", "\\)")
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
    )) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      line_attrs = dplyr::case_when(
        stringr::str_detect(line, "=") ~ stringr::str_split(line, "\\(|,") %>%
          unlist() %>%
          dplyr::tibble(line_attrs = .) %>%
          tidyr::separate(line_attrs, c("attr_name", "attr_val"), sep = '="') %>%
          dplyr::mutate(attr_name = dplyr::case_when(
            !is.na(attr_val) &
              make.names(str_trim(attr_name)) != stringr::str_trim(attr_name) ~ paste0("`", stringr::str_trim(attr_name), "`"),
            T ~ stringr::str_trim(attr_name)
          )) %>%
          dplyr::mutate(attr_string = dplyr::case_when(
            !is.na(attr_name) & !is.na(attr_val) ~ paste0(attr_name, '="', attr_val),
            T ~ attr_name
          )) %>%
          dplyr::pull(attr_string) %>%
          paste(collapse = ", ") %>%
          stringr::str_replace("=NA,", "\\(") %>%
          stringr::str_replace(", ", "(") %>%
          stringr::str_trim() %>%
          stringr::str_squish()
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(line = dplyr::case_when(
      !is.na(line_attrs) ~ line_attrs,
      T ~ line
    )) %>%
    dplyr::mutate(line = dplyr::case_when(
      rowid %in% self_enclosing ~ stringi::stri_reverse(line) %>%
        stringr::str_replace(",", "\\)") %>%
        stringi::stri_reverse(.),
      T ~ line
    ))
}
