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

  lines <- capture.output(formal_args) %>%
    tibble(line = .) %>%
    filter(line != "NULL") %>%
    mutate(line = str_replace(line, "function ", fun_name)) %>%
    pull(line) %>%
    style_text()

    write_clip(lines)
    message(paste(fun_name,"parameters copied to clipboard"))
    cat(lines,sep="\n")

}





