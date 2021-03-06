#' tries to fix unnamed package functions
#'
#' @param f_path file path
#' @return rewrites file with changes
#' @export
#' @examples
#' attempt_formal(here::here("R","new_project.R"))
#'
attempt_formal <- function(f_path) {
  replace_dat <- NCmisc::list.functions.in.file(f_path) %>%
    as.data.frame() %>%
    tibble::rownames_to_column() %>%
    dplyr::tibble() %>%
    stats::setNames(c("rowname", "funs")) %>%
    tidyr::unnest(cols = c(funs)) %>%
    tidyr::separate(rowname, c("other", "default"), sep = '"', extra = "drop") %>%
    tidyr::gather(other, package, -funs) %>%
    dplyr::select(package, funs) %>%
    dplyr::filter(
      stringr::str_detect(package, "package:"),
      !stringr::str_detect(package, "package:stats|package:graphics|package:grDevices|package:utils|package:datasets|package:methods|package:base"),
      !stringr::str_detect(package, stringr::str_replace(
        list.files(pattern = ".Rproj$"),
        paste0(".", tools::file_ext(list.files(pattern = ".Rproj$"))), ""
      ))
    ) %>%
    dplyr::mutate(
      funs = paste0(" ", funs, "\\("),
      new_fun = paste0(" ", package, "::", stringr::str_trim(funs)) %>%
        stringr::str_replace_all("package:", "")
    ) %>%
    dplyr::select(-package)

  if (nrow(replace_dat) > 0) {
    replace_dat <- replace_dat %>%
      dplyr::mutate_all(str_trim) %>%
      dplyr::mutate_all(funs(paste0("\\(", .))) %>%
      dplyr::bind_rows(replace_dat)


    replace_string <- replace_dat %>%
      dplyr::pull(new_fun) %>%
      purrr::set_names(dplyr::pull(replace_dat, funs))

    readr::read_lines(f_path) %>%
      stringr::str_replace_all(replace_string) %>%
      readr::write_lines(f_path)
  }
}
