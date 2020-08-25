##betting odds:

matchData <- dbGetQuery(con,"SELECT *
                        FROM Match
                        JOIN Country on Country.id = Match.country_id
                        JOIN League on League.id = Match.league_id")
removeCols <- c("goal","shoton","shotoff","foulcommit","card", 
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
  names(dataset)[4] <- paste0("add",bettinghouse, "H")
  names(dataset)[5] <-  paste0("add",bettinghouse, "D")
  names(dataset)[6] <-  paste0("add",bettinghouse, "A")
  
  dataset <- cbind(dataset, implied_probabilities(dataset[1:3], method="shin")$probabilities)
  names(dataset)[7] <- paste0("shin",bettinghouse, "H")
  names(dataset)[8] <- paste0("shin",bettinghouse, "D")
  names(dataset)[9] <- paste0("shin",bettinghouse, "A")
  
  dataset <- cbind(dataset, implied_probabilities(dataset[1:3], method="wpo")$probabilities)
  names(dataset)[10] <- paste0("cum",bettinghouse, "H")
  names(dataset)[11] <- paste0("cum",bettinghouse, "D")
  names(dataset)[12] <- paste0("cum",bettinghouse, "A")
  
  dataset <- cbind(dataset, implied_probabilities(dataset[1:3], method="power")$probabilities)
  names(dataset)[13] <- paste0("power",bettinghouse, "H")
  names(dataset)[14] <- paste0("power",bettinghouse, "D")
  names(dataset)[15] <- paste0("power",bettinghouse, "A")
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
results <- cbind(matchData[1:11], B365,
                 matchData[12:14], BW,
                 matchData[15:17], IW,
                 matchData[18:20], LB,
                 matchData[21:23], PS,
                 matchData[24:26], WH,
                 matchData[27:29], SJ,
                 matchData[30:32], VC,
                 matchData[33:35], GB,
                 matchData[36:38], BS,
                 matchData[40])


saveRDS(matchData, results, file = "odds.rds")
readRDS(file = "odds.rds")










