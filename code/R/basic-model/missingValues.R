##########################################
#EDA:
##########################################


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
11762/(11762+14217)
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
notMissing <- table(matchData$name[!is.na(matchData$goal)])
missing <- table(matchData$name[is.na(matchData$goal)])
total <- dplyr::bind_rows(notMissing, missing)
total[is.na(total)] <- 0
total[3,] <- (total[1,] / (total[1,] + total[2,]))*100
total
total$label <- NULL
total$label<- c("Data", "Missing","%")
