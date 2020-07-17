#' add recession bars to plotly object
#'
#' @param plot plot_ly object
#' @param extra extra shapes to be added to plot
#' @export
#' @examples
#' economics %>%
#' plot_ly(x = ~date,
#'         y = ~uempmed) %>%
#'   add_lines() %>%
#'   add_recession_bars()


add_recession_bars <- function(plot,extra = NULL) {
  recession_bars <- function(peak, trough) {
    list(
      type = "rect",
      fillcolor = "grey",
      line = list(color = "grey"),
      opacity = 0.2,
      x0 = as.character(peak),
      x1 = as.character(trough),
      xref = "x",
      y0 = 0, y1 = 1, yref = "paper"
    )
  }

  recessions.trim <- filter(recessions.df, Peak >= min(plotly_data(plot)$date, na.rm = T))

  shapes <- map2(recessions.trim$Peak, recessions.trim$Trough, recession_bars)

  if (!is.null(extra)) {
    shapes[[length(shapes) + 1]] <- extra
  }

  plot %>%
    layout(shapes = shapes)
}




recessions.df <- read.table(textConnection(
  "Peak, Trough
1857-06-01, 1858-12-01
1860-10-01, 1861-06-01
1865-04-01, 1867-12-01
1869-06-01, 1870-12-01
1873-10-01, 1879-03-01
1882-03-01, 1885-05-01
1887-03-01, 1888-04-01
1890-07-01, 1891-05-01
1893-01-01, 1894-06-01
1895-12-01, 1897-06-01
1899-06-01, 1900-12-01
1902-09-01, 1904-08-01
1907-05-01, 1908-06-01
1910-01-01, 1912-01-01
1913-01-01, 1914-12-01
1918-08-01, 1919-03-01
1920-01-01, 1921-07-01
1923-05-01, 1924-07-01
1926-10-01, 1927-11-01
1929-08-01, 1933-03-01
1937-05-01, 1938-06-01
1945-02-01, 1945-10-01
1948-11-01, 1949-10-01
1953-07-01, 1954-05-01
1957-08-01, 1958-04-01
1960-04-01, 1961-02-01
1969-12-01, 1970-11-01
1973-11-01, 1975-03-01
1980-01-01, 1980-07-01
1981-07-01, 1982-11-01
1990-07-01, 1991-03-01
2001-03-01, 2001-11-01
2007-12-01, 2009-06-01"
),sep = ",",colClasses = c("Date", "Date"), header = TRUE)


