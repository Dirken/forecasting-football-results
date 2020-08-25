##betting odds:

matchData <- dbGetQuery(con,"SELECT *
                        FROM Match
                        JOIN Country on Country.id = Match.country_id
                        JOIN League on League.id = Match.league_id")
removeCols <- c("goal","shoton","shotoff","foulcommit","card", "corner",
                "cross", "possession", "id..118" ,"id..116", "country_id..119", "country_id",
                "name", "id", "country_id", "league_id")

matchData <- matchData %>%
  select(-starts_with("home_player_")) %>%
  select(-starts_with("away_player_")) %>%
  select(-one_of(removeCols))
  
matchData$winner <- ifelse(matchData$home_team_goal == matchData$away_team_goal, 
                           "Draw", 
                           ifelse(matchData$home_team_goal > matchData$away_team_goal, 
                                  "Home Win",
                                  "Away Win"
                           )
                    )

#######################
#Overround
#######################
overroundGen <- function(dataset, bettingHouse){
  
  dataset <- cbind(dataset, implied_probabilities(dataset, method="basic")$probabilities)
  names(dataset)[4] <- paste0("add",bettingHouse, "H")
  names(dataset)[5] <-  paste0("add",bettingHouse, "D")
  names(dataset)[6] <-  paste0("add",bettingHouse, "A")
  
  dataset <- cbind(dataset, implied_probabilities(dataset[1:3], method="shin")$probabilities)
  names(dataset)[7] <- paste0("shin",bettingHouse, "H")
  names(dataset)[8] <- paste0("shin",bettingHouse, "D")
  names(dataset)[9] <- paste0("shin",bettingHouse, "A")
  
  dataset <- cbind(dataset, implied_probabilities(dataset[1:3], method="wpo")$probabilities)
  names(dataset)[10] <- paste0("cum",bettingHouse, "H")
  names(dataset)[11] <- paste0("cum",bettingHouse, "D")
  names(dataset)[12] <- paste0("cum",bettingHouse, "A")
  
  dataset <- cbind(dataset, implied_probabilities(dataset[1:3], method="power")$probabilities)
  names(dataset)[13] <- paste0("power",bettingHouse, "H")
  names(dataset)[14] <- paste0("power",bettingHouse, "D")
  names(dataset)[15] <- paste0("power",bettingHouse, "A")
  names(dataset)[1] <- paste0(bettingHouse,"H")
  names(dataset)[2] <- paste0(bettingHouse,"D")
  names(dataset)[3] <- paste0(bettingHouse,"A")
  
  return(dataset)
}


B365 <- data.frame(matchData$B365H,matchData$B365D, matchData$B365A)
B365 <- overroundGen(B365, "B365")
BW <- data.frame(matchData$BWH,matchData$BWD, matchData$BWA)
BW <- overroundGen(BW, "BW")
IW <- data.frame(matchData$IWH,matchData$IWD, matchData$IWA)
IW <- overroundGen(IW, "IW")
LB <- data.frame(matchData$LBH,matchData$LBD, matchData$LBA)
LB <- overroundGen(LB, "LB")
PS <- data.frame(matchData$PSH,matchData$PSD, matchData$PSA)
PS <- overroundGen(PS, "PS")
WH <- data.frame(matchData$WHH,matchData$WHD, matchData$WHA)
WH <- overroundGen(WH, "WH")
SJ <- data.frame(matchData$SJH,matchData$SJD, matchData$SJA)
SJ <- overroundGen(SJ, "SJ")
VC <- data.frame(matchData$VCH,matchData$VCD, matchData$VCA)
VC <- overroundGen(VC, "VC")
GB <- data.frame(matchData$GBH,matchData$GBD, matchData$GBA)
GB <- overroundGen(GB, "GB")
BS <- data.frame(matchData$BSH,matchData$BSD, matchData$BSA)
BS <- overroundGen(BS, "BS")

#######################
# Data Creation
#######################
results <- cbind(matchData$season, 
                 matchData$stage, 
                 matchData$date, 
                 matchData$home_team_api_id, 
                 matchData$away_team_api_id, 
                 matchData$home_team_goal, 
                 matchData$away_team_goal,  
                 B365, BW, IW, LB, PS, WH, SJ, VC, GB, BS, matchData$winner)


saveRDS(matchData, results, file = "odds.rds")
readRDS(file = "odds.rds")










