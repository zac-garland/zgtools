#' Pipe
#'
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe
#' @export
#' @param lhs,rhs A visualisation and a function to apply to it
#' @examples
NULL




# Like base::paste, but converts all string args to UTF-8 first.
paste8 <- function(..., sep = " ", collapse = NULL) {
  args <- c(
    lapply(list(...), enc2utf8),
    list(
      sep = if (is.null(sep)) sep else enc2utf8(sep),
      collapse = if (is.null(collapse)) collapse else enc2utf8(collapse)
    )
  )

  do.call(paste, args)
}
