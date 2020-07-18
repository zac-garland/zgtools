#' plotly dark theme
#'
#' @param plot plotly plot
#' @export
#' @examples
#' mtcars %>%
#'   plot_ly(x = ~cyl, y = ~mpg) %>%
#'   plotly_elegant(
#'     xaxis = list(title = "hey"),
#'     yaxis = list(side = "right")
#'   )
plotly_elegant <- function(plot, ...) {
  plot %>%
    plotly::layout(
      xaxis = list(title = ""),
      yaxis = list(title = ""),
      font = list(color = "#ffffff"),
      plot_bgcolor = "#1f1f1e",
      paper_bgcolor = "#1f1f1e",
      margin = 3
    ) %>%
    plotly::layout(...)
}
