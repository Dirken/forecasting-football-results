#########################################################
#General
#########################################################
shell.exec("folders.bat")
source("libraries.R")
library("viridis")
source("loadingDB.R")
#source("missingValues.R")

#########################################################
#Base Model
#########################################################
#factor analysis: does possesion imply win? Do red cards imply more wins?
source("factorStudies.R")
source("squad.R")
source("transformations.R")


#########################################################
# Maybe useful? Don't think so
######################################################### 
#Fifa analysis of the data
#player + player_attrs
source("playerData.R")

#team + team_attrs
source("teamsData.R")

