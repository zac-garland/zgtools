register_keywords <- function(functions_to_include) {
  c(
    "<script src=\"https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.15.6/highlight.min.js\"></script>",
    "<script> hljs.registerLanguage(\"r\", function(e) {", "var r = \"([a-zA-Z]|\\\\.[a-zA-Z.])[a-zA-Z0-9._]*\";",
    "return {", "c: [e.HCM, {", "b: r,", "l: r,", "k: {",
    paste0("keyword: \"function if in break next repeat else for return switch while try tryCatch stop warning require library attach detach source setMethod setGeneric setGroupGeneric setClass ... ", functions_to_include, "\","),
    "literal: \"NULL NA TRUE FALSE T F Inf NaN NA_integer_|10 NA_real_|10 NA_character_|10 NA_complex_|10\"",
    "},", "r: 0", "},{cN:\"pipe\",", "b:\"%>%|\\\\+|\\\\(|\\\\)\",r:0}, {", "cN: \"number\",", "b: \"0[xX][0-9a-fA-F]+[Li]?\\\\b\",",
    "r: 0", "}, {", "cN: \"number\",", "b: \"\\\\d+(?:[eE][+\\\\-]?\\\\d*)?L\\\\b\",",
    "r: 0", "}, {", "cN: \"number\",", "b: \"\\\\d+\\\\.(?!\\\\d)(?:i\\\\b)?\",",
    "r: 0", "}, {", "cN: \"number\",", "b: \"\\\\d+(?:\\\\.\\\\d*)?(?:[eE][+\\\\-]?\\\\d*)?i?\\\\b\",",
    "r: 0", "}, {", "cN: \"number\",", "b: \"\\\\.\\\\d+(?:[eE][+\\\\-]?\\\\d*)?i?\\\\b\",",
    "r: 0", "}, {", "b: \"`\",", "e: \"`\",", "r: 0", "}, {", "cN: \"string\",",
    "c: [e.BE],", "v: [{", "b: '\"',", "e: '\"'", "}, {", "b: \"'\",",
    "e: \"'\"", "}]", "}]", "}", "}); </script>", "<script>hljs.initHighlightingOnLoad();</script>",
    ""
  ) %>%
    shiny::HTML() %>%
    shiny::tags$script()
}

#' registers r functions in env with highlight.js
#'
#' @param ... packages to load and highlight
#' @export
#' @examples
#' highlight_load(tidyverse, stats)
highlight_load <- function(...) {
  packages <- as.character(match.call(expand.dots = FALSE)[[2]])

  pacman::p_load(
    char = packages
  )

  package_names <- .packages()

  functions_in_file <- paste0("package:", package_names) %>%
    lapply(ls) %>%
    unlist() %>%
    as.character() %>%
    unique()

  check_name <- function(character) {
    if (character == make.names(character)) {
      character
    } else {
      ""
    }
  }
  paste(map_chr(functions_in_file, check_name), collapse = " ") %>%
    stringr::str_squish() %>%
    register_keywords()
}
