#######################
#Data cleaning
#######################

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
  select(-one_of(removeCols)) %>%
  rename(
    league_name = name..120
  )
  
matchData$winner <- ifelse(matchData$home_team_goal == matchData$away_team_goal, 
                           "Draw", 
                           ifelse(matchData$home_team_goal > matchData$away_team_goal, 
                                  "Home Win",
                                  "Away Win"
                           )
                    )

removeGoals <- c("home_team_goal", "away_team_goal")
matchData <- matchData %>% select(-one_of(removeGoals))
matchData$date <- as.Date(matchData$date)
cols <- c("winner", "league_name", "season", "match_api_id", "home_team_api_id", "away_team_api_id" )
matchData[cols] <- lapply(matchData[cols], factor) 
sapply(matchData, class)

#######################
#Imputations
#######################

matchData <- matchData[rowSums(is.na(matchData[9:38])) != ncol(matchData[9:38]), ]
matrixplot(matchData %>% select(-matches("*(B365|BW|IW|LB|PS|WH|SJ|VC|GB|BS)(A|D)")))

#######################
#Approach #1: mice approach, should check which is the best method, seed and m and maxit
#######################
miceOutput <- matchData
miceImputation <- mice(matchData[7:36],m=5,maxit=1,meth='pmm',seed=500)

#Values obtained of the imputations:
summary(miceImputation)

miceImputation$imp$PSH

densityplot(miceImputation)
stripplot(miceImputation)

#######################
#Approach #2: KNN imputation. should check the K
#######################
knnImputedData <- knnImputation(data = matchData[7:36], k = 5, scale = T, meth="weighAvg")
summary(knnImputedData)

#######################
#Approach #3: Random Forest imputation.
#######################
randomForestImputed <- missForest(matchData[7:36])$ximp #takes way too much wtf

#######################
#Approach #4: imputePCA. should check the ncp
#######################
pcaOutput <- imputePCA(matchData[7:36], ncp=4)
pcaImputation <- as.data.frame(pcaImputation$completeObs)



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


#######################
# Data Creation
#######################
#datasets <- list(matchData, miceImputation, knnImputedData, randomForestImputed, pcaImputation)
datasets <- list(matchData, miceImputation, knnImputedData)
for(index in datasets){
  B365 <- data.frame(index$B365H,index$B365D, index$B365A)
  B365 <- overroundGen(B365, "B365")
  BW <- data.frame(index$BWH,index$BWD, index$BWA)
  BW <- overroundGen(BW, "BW")
  IW <- data.frame(index$IWH,index$IWD, index$IWA)
  IW <- overroundGen(IW, "IW")
  LB <- data.frame(index$LBH,index$LBD, index$LBA)
  LB <- overroundGen(LB, "LB")
  PS <- data.frame(index$PSH,index$PSD, index$PSA)
  PS <- overroundGen(PS, "PS")
  WH <- data.frame(index$WHH,index$WHD, index$WHA)
  WH <- overroundGen(WH, "WH")
  SJ <- data.frame(index$SJH,index$SJD, index$SJA)
  SJ <- overroundGen(SJ, "SJ")
  VC <- data.frame(index$VCH,index$VCD, index$VCA)
  VC <- overroundGen(VC, "VC")
  GB <- data.frame(index$GBH,index$GBD, index$GBA)
  GB <- overroundGen(GB, "GB")
  BS <- data.frame(index$BSH,index$BSD, index$BSA)
  BS <- overroundGen(BS, "BS")
  index <- cbind(season = index$season,
                 stage = index$stage,
                 date = index$date,
                 match_api_id = index$match_api_id,
                 home_team_goal = index$home_team_goal,
                 away_team_goal = index$away_team_goal,  
                 B365, 
                 BW, 
                 IW, 
                 LB, 
                 PS, 
                 WH, 
                 SJ, 
                 VC, 
                 GB, 
                 BS, 
                 winner = index$winner)
}





saveRDS(matchData, results, file = "odds.rds")
readRDS(file = "odds.rds")










