library(devtools)
library(yaml)
library(purrr)
library(magrittr)

# Set up working directory
dir <- getwd()

# Load the DESCRIPTION YAML file
descFile <- dir %>%
  paste0("/DESCRIPTION") %>%
  yaml::yaml.load_file()

descFileNames <- names(descFile)
if (descFileNames %>% purrr::has_element('Version')) {
  ver <- descFile[['Version']]
} else {
  message(' ## Cannot find a valid _Version_ key in the DESCRIPTION file --')
  stop(' ## STOPPING --')
}

# Find the current tags from github
current <- 'git tag' %>%
  system(intern = TRUE) %>%
    gsub(
      pattern = 'v',
      replacement = ''
    )
current %<>% `[`(current %>% length)

# Split version in description
verSplit <- ver %>%
  strsplit(split = '[.]') %>%
  purrr::flatten_chr()

# Split current git tag
currentSplit <- current %>%
  strsplit(split = '[.]') %>%
  purrr::flatten_chr()

# Get their respective lengths
verLen <- verSplit %>% length
currentLen <- currentSplit %>% length
diffLen <- verLen %>%
  `-`(currentLen) %>%
  abs

# Align the lengths if they are off by major . minor . path versioning style
if (diffLen != 0) {
  appendVals <- "0" %>% rep(diffLen %>% abs)
  if (verLen > currentLen) currentSplit %<>% c(appendVals) else verSplit %<>% c(appendVals)
}

# Compare the two once they have been cleaned up
compare <- utils::compareVersion(
  a = paste(verSplit, collapse = '.'),
  b = paste(currentSplit, collapse = '.')
)

# Print to screen what is about to happen
if (compare == 0) {
  message(' ## Version has not changed between current tag and DESCRIPTION file--')
  stop(' ## STOPPING -- ')
} else if (compare < 0) {
  message(' ## Version is somehow lower in the DESCRIPTION file --')
  stop(' ## STOPPING -- ')
}

# Once versioning has been approved by the checks then create the tar and move it to /tagged/
devtools::build(
  pkg = dir,
  binary = TRUE
)

# ... find the built tar
latestRelease <- list.files(
  path = '../',
  pattern = '*.tar.gz'
)

# ... rename file to latest release and move to /tagged/
renameSuccess <- file.rename(
  from = paste0('../', latestRelease),
  to = paste0('tagged/', latestRelease)
)

# If successful then print to screen
if (renameSuccess) {
  message(paste0(' ## Tagged latest release successfully from [ ', current, ' ] to [ ', ver, ' ] -- '))
  message(paste0(' ## Double check the versions are as required and complete the tagging on github! -- '))
} else {
  message(' ## WARNING! Could not tag release -- ')
}
