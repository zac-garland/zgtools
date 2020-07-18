git_commit_push <- function(message = "marginal change",...){

  gert::git_commit_all(message = message,...)
  gert::git_push( )

}


