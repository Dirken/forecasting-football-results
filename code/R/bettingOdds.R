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



matchData <- matchData[rowSums(is.na(matchData[9:38])) != ncol(matchData[9:38]), ]
matrixplot(matchData %>% select(-matches("*(B365|BW|IW|LB|PS|WH|SJ|VC|GB|BS)(A|D)")))

#######################
#Imputations
#######################

#######################
#Approach #1: mice approach, should check which is the best method, seed and m and maxit
#######################
miceOutput <- matchData
miceImputation <- mice(matchData[7:36],m=5,maxit=5,meth='pmm',seed=500)

#Values obtained of the imputations:
summary(miceImputation)

miceImputation$imp$PSH

miceOutput[7:36] <- mice::complete(miceImputation, 1)
miceOutput
densityplot(miceImputation)
stripplot(miceImputation)
saveRDS(miceOutput, "miceOutput-noOverround.rds")
readRDS("miceOutput-noOverround.rds")
#######################
#Approach #2: KNN imputation. should check the K
#######################
knnImputedData <- matchData
start.time <- Sys.time()
knnImputedData[7:36] <- knnImputation(data = matchData[7:36], k = 5, scale = T, meth="weighAvg")
end.time <- Sys.time()
time.taken <- end.time - start.time
summary(knnImputedData)
saveRDS(knnImputedData, "knnImputedData-noOverround.rds")
readRDS("knnImputedData-noOverround.rds")
#######################
#Approach #3: Random Forest imputation.
#######################
#start.time <- Sys.time()
#randomImputation <- matchData
#randomForestImputed <- missForest(matchData[7:36])$ximp #takes way too much wtf
#randomImputation[7:36] <- randomForestImputed
#end.time <- Sys.time()
#time.taken2 <- end.time - start.time
#time.taken2
#saveRDS(randomImputation, "randomImputation.rds")
#######################
#Approach #4: imputePCA. should check the ncp
#######################
pcaOutput <- matchData
start.time <- Sys.time()
pcaOutput[7:36] <- imputePCA(matchData[7:36], ncp=4)
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)
saveRDS(pcaOutput, "pcaOutput-noOverround.rds")
readRDS("pcaOutput-noOverround.rds")
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
#datasets <- list(matchData, miceOutput, knnImputedData, randomForestImputed, pcaImputation)
datasets <- list(matchData, miceOutput, knnImputedData, pcaOutput)
counter <- 0
for(index in datasets){
  counter = counter + 1
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

  datasets[[counter]] <- as.data.frame(list(season = matchData$season,
                 stage = matchData$stage,
                 date = matchData$date,
                 match_api_id = matchData$match_api_id,
                 league_name = matchData$league_name,
                 home_team_api_id = matchData$home_team_api_id,
                 away_team_api_id = matchData$away_team_api_id,
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
                 winner = matchData$winner))
}


matchData <- datasets[[1]]
miceOutput <-  datasets[[2]]
knnImputedData <- datasets[[3]]
pcaOutput <- datasets[[4]]



saveRDS(matchData, "matchData.rds")
saveRDS(miceOutput, "miceOutput.rds")
saveRDS(knnImputedData, "knnImputedData.rds")
saveRDS(pcaOutput, "pcaOutput.rds")
save.image(file = "bettingOdds.RData")
readRDS("matchData.rds")
readRDS("miceOutput.rds")
readRDS("knnImputedData.rds")
readRDS("pcaOutput.rds")
load.image("bettingOdds.RData")


