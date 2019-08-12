#' @title Rename Columns
#'
#' @export


rename_columns <- function(d.set, mapping = "") {
  # Read the mappings from yaml
  myMappings <- system.file('mappings', 'details.yaml', package = 'footballstats') %>%
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

  # Rename those columns
  d.set %<>% dplyr::rename(!!!newList)

  return(d.set %>% dplyr::select(newList %>% names))
}
