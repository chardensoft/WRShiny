
pullFromGit <- function() {
  system("git pull --no-rebase")
  
  return("pulled from git")
}