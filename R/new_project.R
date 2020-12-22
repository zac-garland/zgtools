#' Easy function to create R package from a function
#'
#' @param package_name package name
#' @param author_name "First Last"
#' @param email "email to include"
#' @export
#' @examples
#' new_project(package_name, author_name = "First Last", email = "")
new_project <- function(package_name, author_name = "First Last", email = "") {

  package_name <- stringr::str_to_lower(package_name)

  proj_path <- path.expand("~/r_projects")

  if (!dir.exists(proj_path)) dir.create(proj_path)

  new_dir <- file.path(proj_path, make.names(stringr::str_to_lower(package_name)))

  package_name <- stringr::str_replace_all(package_name, "_", ".")

# need to add in folder creation, inst, notes, data

  author_name <- strsplit(author_name, " ") %>%
    unlist() %>%
    shQuote() %>%
    paste(collapse = ",")

  usethis::create_package(new_dir,
    fields = list(
      Package = package_name,
      `Authors@R` = paste0("person(", author_name, ", email = ", shQuote(email), ', role = c("aut", "cre"))'),
      License = "MIT + file LICENSE",
      Language = "es"
    )
  )
}

