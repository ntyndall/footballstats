library(devtools)
library(yaml)
library(purrr)
library(magrittr)

dir <- getwd()

descFile <- yaml::yaml.load_file(
  input = paste0(dir, '/DESCRIPTION'))

descFileNames <- names(descFile)
if (descFileNames %>% purrr::has_element('Version')) {
  ver <- descFile[['Version']]
} else {
  message(' -- Cannot find a valid _Version_ key in the DESCRIPTION file --')
  stop(' END (unsuccessful)')
}

current <- 'git tag' %>%
  system(
    intern = TRUE) %>%
    gsub(
      pattern = 'v',
      replacement = '')

verSplit <- (ver %>% strsplit(split = '[.]'))[[1]]
currentSplit <- (current %>% strsplit(split = '[.]'))[[1]]

verLen <- verSplit %>% length()
currentLen <- currentSplit %>% length()
if (verLen > currentLen) {
  currentSplit <- c(currentSplit, rep(0, (verLen - currentLen)))
} else if (verLen < currentLen) {
  verSplit <- c(verSplit, rep(0, (currentLen - verLen)))
}

compare <- utils::compareVersion(
  a = paste(verSplit, collapse = '.'),
  b = paste(currentSplit, collapse = '.'))

if (compare == 0) {
  message(' -- Version hasnt changed between current tag and DESCRIPTION file--')
  stop(' END (unsuccessful) ')
} else if (compare < 0) {
  message(' -- Version is somehow lower in the DESCRIPTION file --')
  stop(' END (unsuccessful) ')
}

# Once versioning has been approved by the checks then create the tar and move it to /tagged/
devtools::build(
  pkg = dir,
  binary = TRUE)

latestRelease <- list.files(
  path = '../',
  pattern = '*.tar.gz')

renameSuccess <- file.rename(
  from = paste0('../', latestRelease),
  to = paste0('tagged/', latestRelease))

if (renameSuccess) {
  message(paste0(' -- Tagged latest release successfully from [ ', current, ' ] to [ ', ver, ' ] -- '))
  message(paste0(' -- Double check the versions are as required and complete the tagging on github! -- '))
} else {
  message(' -- Warning! Could not tag release -- ')
}
