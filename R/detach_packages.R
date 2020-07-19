#' detaches all non base packages in environment
#'
#' @return nothing
#' @seealso https://stackoverflow.com/questions/7505547/detach-all-packages-while-working-in-r
#' @export
#' @examples
#' detach_packages()



detach_packages <- function(...) {
  base_packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")

  package_list <- search()[ifelse(unlist(gregexpr("package:", search())) == 1, TRUE, FALSE)]

  package_list <- setdiff(package_list, base_packages)

  if (length(package_list) > 0) for (package in package_list) detach(package, character.only = TRUE)
}
