#' Easy function to create R package from a function
#'
#' @param package_name package name
#' @param author_name "First Last"
#' @param email "email to include"
#' @export
#' @examples
#' new_project(package_name, author_name = "First Last", email = "")

new_project <- function(package_name,author_name = "First Last",email = ""){
  proj_path <- path.expand("~/r_projects")
  if (!dir.exists(proj_path)) dir.create(proj_path)
  new_dir <- file.path(proj_path,str_replace_all(str_to_lower(package_name),"[[:punct:]]", ""))

  author_name = strsplit(author_name," ") %>%
    unlist() %>%
    shQuote() %>%
    paste(collapse=",")

  usethis::create_package(new_dir,
    fields = list(
      Package = basename(new_dir),
      `Authors@R` = paste0('person(',author_name,', email = ',shQuote(email),', role = c("aut", "cre"))'),
      License = "MIT + file LICENSE",
      Language = "es"
    )
  )
}





