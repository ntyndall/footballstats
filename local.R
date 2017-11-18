library(devtools)

# Source all three functions
# reload, build_library, and destroy_library
# Example : 
# reload(dir = '~/Desktop/football-project/footballstats')

reload <- function(dir) {
  destroy_library()
  build_library(dir = dir)
}


build_library <- function(dir) {

setwd(dir)
devtools::build(pkg = getwd())
builtPkg <- dir(path = '../', pattern = '*.tar.gz')

file.rename(from = paste0('../', builtPkg), to = paste0(getwd(), '/', builtPkg))

install.packages(builtPkg, 
                 repos = NULL, 
                 type = "source")

file.remove(builtPkg)
library(footballstats)
}

destroy_library <- function() {
  # Is package loaded?
  if ('footballstats' %in% .packages()) {
    detach("package:footballstats", unload = TRUE)
  }
  
  # Does the package exist at all?
  pkgExists <- require('footballstats')
 
  if (pkgExists) {
    remove.packages('footballstats')
  }
}
