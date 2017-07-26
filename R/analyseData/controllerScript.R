#
# analysis controller script
#

# (1) Create a new redis connection to be on the safe side!
analyseData <- redux::hiredis(host = 'localhost', port = 6379, db = 9)
exampleCompetitionID <- 1102

# (2) Get match data
matchData <- recreateMatchData(competitionID = exampleCompetitionID, 
                               seasonStarting = 2015,
                               redisData = redis)

# (3) Generate team form based on match data
generateTeamForm(competitionID = exampleCompetitionID,
                 redisData = redis)

