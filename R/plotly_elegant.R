#' plotly dark theme
#'
#' @param plot plotly plot
#' @param margin plot margin
#' @param background plot background
#' @param color font color
#' @export
#' @examples
#' mtcars %>%
#'   plot_ly(x = ~cyl, y = ~mpg) %>%
#'   plotly_elegant(
#'     xaxis = list(title = "hey"),
#'     yaxis = list(side = "right")
#'   )
plotly_elegant <- function(plot, ..., margin = 3, background = "#1f1f1e", color = "#ffffff") {
  plot %>%
    plotly::layout(
      xaxis = list(title = ""),
      yaxis = list(title = ""),
      font = list(color = color,family = "Century Gothic"),
      plot_bgcolor = background,
      paper_bgcolor = background,
      margin = margin
    ) %>%
    plotly::layout(...)
}
