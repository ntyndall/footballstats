#
#  Run script to populate Redis.
#

source(paste0(getwd(), '/Desktop/football-project/footballstats/R/MainFunctions/storeData.R'))
source(paste0(getwd(), '/Desktop/football-project/footballstats/R/UtilityFunctions/initialize.R'))

# Load competitions and run the functionality below. 
# (Figure out competition ID's with a single GET request first!)
#competitions <- addCompetitionInfo()
comps <- jsonlite::fromJSON(seasonIDs)
seasonYear <- c(2013, 2014, 2015, 2016)

for (i in 1:nrow(comps)) {
  for (j in 1:length(seasonYear)) {
    print(paste0('looking at season ', seasonYear[j], '/', seasonYear[j] + 1, 
                 ' for competition - ', comps$name[i]))
    storeData(competitionID = comps$id[i], 
              seasonStarting = seasonYear[j],
              updateData = FALSE)
  }
}
