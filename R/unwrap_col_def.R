#' allows you to unwrap or display all of a column
#'
#' @param value direct column ref using dot notation
#' @param ... additional params to pass to reactable
#' @param px_factor pixel factor for col width
#' @param style previously defined style rules
#' @param headerStyle previously defined header rules
#' @export

unwrap_col_def <- function(value, ..., px_factor = 10, style = NULL, headerStyle = NULL) {
    min_width <- max(stringr::str_length(value), na.rm = T) * px_factor

    style <- c(style, list(whiteSpace = "nowrap", minWidth = min_width))
    headerStyle <- c(headerStyle, list(
      minWidth = min_width
    ))

  reactable::colDef(..., style = style, headerStyle = headerStyle)
}
