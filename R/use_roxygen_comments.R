#' creates a roxygen template for functions
#'
#' @param x function name
#' @return copies comment template to clipboard
#' @export
#' @examples
#' use_roxygen_comments(mean)

use_roxygen_comments <- function(x, ...){
  formal_args <- args(x)
  fun_name <- match.call(x)[2] %>%
    as.character()

  lines <- capture.output(formal_args) %>%
    tibble(line = .) %>%
    filter(line != "NULL") %>%
    mutate(line = str_replace(line, "function ", fun_name)) %>%
    pull(line)

  x <- match.fun(x)
  nms <- names(formals(x))
  if(length(nms) > 0) {
    params <- paste0("@param ", nms, " <parameter description>")
  } else {
    params <- character(0)
  }

  txt <- c("<Description>", "", params, "@return <describe returned object>",
           "@seealso <links to other help pages>", "@export","@examples",lines)
  out <- c(paste0("#' ", txt),"")
  write_clip(out)
  message("roxygen template copied to clipboard")
  cat(out, sep = '\n')
  invisible(out)

}
