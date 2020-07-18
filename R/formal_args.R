#' copies and prints default function call
#'
#' @param function_name name of function
#' @return prints function call and copies to clipboard
#' @export
#' @examples
#' formal_args(mean)
formal_args <- function(function_name) {
  formal_args <- args(function_name)
  fun_name <- match.call(function_name)[2] %>%
    as.character()

  lines <- utils::capture.output(formal_args) %>%
    dplyr::tibble(line = .) %>%
    dplyr::filter(line != "NULL") %>%
    dplyr::mutate(line = stringr::str_replace(line, "function ", fun_name)) %>%
    dplyr::pull(line) %>%
    styler::style_text()

  clipr::write_clip(lines)
  message(paste(fun_name, "parameters copied to clipboard"))
  cat(lines, sep = "\n")
}
