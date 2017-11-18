library(devtools)

dir <- getwd()
devtools::build(pkg = dir, 
                binary = TRUE)

latestRelease <- list.files(path = '../', 
                            pattern = '*.tar.gz')

q <- capture.output(file.rename(
  from = paste0('../', latestRelease),
  to = paste0('tagged/', latestRelease))

if (q) {
  print(' -- Tagged latest release -- ')
else {
  print(' -- Warning! Could not tag release -- ')
}
