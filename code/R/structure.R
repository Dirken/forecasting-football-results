source("libraries.R")

#Loading European Football dataset for betting odds and base models and analysing their NA's
source("loadingDB.R")
source("missingValues.R") #need to interpret better aggr plots.
#Are all leagues equal? How does win % holds across leagues? How uniform each league is?
#also we study other factors per league
source("leaguesAnalysis.R")

#########################################################
#Betting Odds model
#########################################################

#predicting odds, are they accurate?
source("bettingOdds.R") #here a bit messy already because i am creating new variables...
source("bettingOddsEDA.R")
source("bettingOddsModelling.R")



#########################################################
#Base Model
#########################################################
#factor analysis: does possesion imply win? Do red cards imply more wins?
source("factorStudies.R")



#Fifa analysis of the data
#player + player_attrs
source("playerData.R")

#team + team_attrs
source("teamsData.R")


#######################################
#Statsbomb model
#######################################
source("explorationSB.R")


