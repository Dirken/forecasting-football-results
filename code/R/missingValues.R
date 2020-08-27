##########################################
#EDA:
##########################################
#do we have a lot of NA's?
numberOfTables <- length(tables)
for(i in 1:numberOfTables){
  
  fileName <- paste0("missingValues/md-pattern", i, ".jpeg")
  jpeg(file=fileName, width = 1000, height = 1000)
  md.pattern(tables[[i]])
  dev.off()
  
  fileName <- paste0("missingValues/matrixplot", i, ".jpeg")
  jpeg(file=fileName, width = 1000, height = 1000)
  matrixplot(tables[[3]]) #◘tables[[3]][86:115] to do matrixplot of betting odds
  dev.off()
  
  fileName <- paste0("missingValues/scattmatrixMiss", i, ".jpeg")
  jpeg(file=fileName, width = 1000, height = 1000)
  scattmatrixMiss(tables[[i]])
  dev.off()
}

#Need to interpret this plots better.
par(resetPar())
fileName <- paste0("missingValues/aggrMatches", ".jpeg")
jpeg(file=fileName, width = 5000, height = 5000)
aggr(match, numbers =T, prop=T, cex.axis=3,cex.numbers=3) #more readable but not the rhs
dev.off()
fileName <- paste0("missingValues/aggrTeams", ".jpeg")
jpeg(file=fileName, width = 5000, height = 5000)
aggr(team, numbers =T, prop=T, cex.axis=3,cex.numbers=3)
dev.off()




#in match table we can see that for the values:
#- Goal 
#- Posession
#- ShotOn
#- ShotOff
#- FoulCommit
#- Card
#- Corner
#We have a lot of missing values but are this missing values generalized across leagues?
#Can we use some league? Or we are going to use another source?

#To see this we can for instance see goals, in which leagues is missing
#table(is.na(match$goal))
#FALSE  TRUE 
#14217 11762 

matchData <- dbGetQuery(con,"SELECT *
                        FROM Match
                        JOIN Country on Country.id = Match.country_id
                        JOIN League on League.id = Match.league_id
                        LEFT JOIN Team AS HT on HT.team_api_id = Match.home_team_api_id
                        LEFT JOIN Team AS AT on AT.team_api_id = Match.away_team_api_id")
is.na(matchData$goal)
matchData$goal[!is.na(matchData$goal)] #idk then why in the plot appear as there is not data xddddd.
length(table(matchData$goal))
table(matchData$name[is.na(matchData$goal)])
#Seems like the data from Spanish and English leagues are well scrapped but others are not.
#Belgium      France       Italy Netherlands      Poland    Portugal    Scotland       Spain Switzerland 
#1728        1014           3        1918        1913        2052        1811           1        1322 