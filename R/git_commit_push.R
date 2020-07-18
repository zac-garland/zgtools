#' helper function for easily committing all and pushing changes to github
#'
#' @param message commit message, defaults to marginal change
#' @export
#' @examples
#' git_commit_push(message = "marginal change", ...)



git_commit_push <- function(message = "marginal change",...){

  gert::git_commit_all(message = message,...)
  gert::git_push( )

}


