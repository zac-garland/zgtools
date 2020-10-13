#' creates a boilerplate tribble based on provided names
#'
#' @param names column names
#' @param nrows fake data number of rows
#' @export
#' @examples
#' create_tribble(names(mtcars), nrows = 4)



create_tribble <- function(names,nrows = 4){
  header <- paste(paste(paste0("~",names),collapse = ", "),"\n")

  base_val <- rep('""',length(names))

  row_vals <- rep(base_val,nrows) %>%
    dplyr::tibble(placeholder = .) %>%
    tibble::rowid_to_column() %>%
    dplyr::mutate(
      placeholder = dplyr::case_when(
        rowid != max(rowid,na.rm = TRUE) ~ paste0(placeholder,","),
        TRUE ~ placeholder
      ),
      placeholder = dplyr::case_when(
        rowid %% length(names) == 0 ~ paste0(placeholder,"\n\n"),
        TRUE ~ placeholder
      )) %>%
    dplyr::pull(placeholder) %>%
    paste(collapse = "")

 out <- glue::glue(
    "tribble(\n",
    paste0(header,","),
    row_vals,

    ")"
  ) %>%
    styler::style_text()



  clipr::write_clip(out)
  message("tribble template copied to clipboard")
  writeLines(out)
  invisible(out)

}






