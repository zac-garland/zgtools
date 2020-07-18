#' creates a roxygen template for functions
#'
#' @param x function name
#' @return copies comment template to clipboard
#' @export
#' @examples
#' use_roxygen_comments(mean)
use_roxygen_comments <- function(x, ...) {
  formal_args <- args(x)
  fun_name <- match.call(x)[2] %>%
    as.character()

  lines <- utils::capture.output(formal_args) %>%
    dplyr::tibble(line = .) %>%
    dplyr::filter(line != "NULL") %>%
    dplyr::mutate(line = stringr::str_replace(line, "function ", fun_name)) %>%
    dplyr::pull(line)

  x <- match.fun(x)
  nms <- names(formals(x))
  if (length(nms) > 0) {
    params <- paste0("@param ", nms, " <parameter description>")
  } else {
    params <- character(0)
  }

  txt <- c(
    "<Description>", "", params, "@return <describe returned object>",
    "@seealso <links to other help pages>", "@export", "@examples", lines
  )
  out <- c(paste0("#' ", txt), "")
  clipr::write_clip(out)
  message("roxygen template copied to clipboard")
  cat(out, sep = "\n")
  invisible(out)
}
