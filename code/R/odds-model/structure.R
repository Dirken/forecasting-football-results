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
source("bettingOdds.R") 
source("bettingOddsEDA.R")
source("bettingOddsOutliers.R")
source("bettingOddsDimensions.R")
source("bettingOddsModelling.R")


