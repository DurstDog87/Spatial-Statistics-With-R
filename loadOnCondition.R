###loadOnCondition###
#takes a package name as a string and installs and loads it if not already loaded
#Arguments:
#pkg = string

loadOnCondition <- function(pkg) {
  if(!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    require(pkg, character.only = TRUE)
  }
}
