#' @title Rename data
#'
#' @export


rename_data <- function(d.set, mapping = "") {
  # Read the mappings from yaml
  myMappings <- "~/Documents/footballstats/inst/mappings/details.yaml" %>%
    yaml::yaml.load_file() %>%
    `[[`(mapping)

  # Only use the mapping values that are present in the data set
  usedNames <- d.set %>%
    names %>%
    `[`(d.set %>% names %in% (myMappings %>% names))

  # Filter out those that dont need changed
  myMappings %<>% `[`(usedNames)

  # Switch the names and values around in the list now that they are unique
  newList <- myMappings %>%
    names %>%
    as.list

  # New names for the list
  names(newList) <- myMappings %>%
    as.character

  # Create a named vector for dplyr to use
  newList %<>% unlist

  # Return the data set with renamed columns
  return(d.set %>% dplyr::rename(!!!newList))
}
