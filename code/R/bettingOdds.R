##betting odds:

matchData <- dbGetQuery(con,"SELECT *
                        FROM Match
                        JOIN Country on Country.id = Match.country_id
                        JOIN League on League.id = Match.league_id")


matchData$home_player_X1 <- NULL
matchData$home_player_X2 <- NULL
matchData$home_player_X3 <- NULL
matchData$home_player_X4 <- NULL
matchData$home_player_X5 <- NULL
matchData$home_player_X6 <- NULL
matchData$home_player_X7 <- NULL
matchData$home_player_X8 <- NULL
matchData$home_player_X9 <- NULL
matchData$home_player_X10 <- NULL
matchData$home_player_X11 <- NULL
matchData$home_player_Y1 <- NULL
matchData$home_player_Y2 <- NULL
matchData$home_player_Y3 <- NULL
matchData$home_player_Y4 <- NULL
matchData$home_player_Y5 <- NULL
matchData$home_player_Y6 <- NULL
matchData$home_player_Y7 <- NULL
matchData$home_player_Y8 <- NULL
matchData$home_player_Y9 <- NULL
matchData$home_player_Y10 <- NULL
matchData$home_player_Y11 <- NULL
matchData$away_player_X1 <- NULL
matchData$away_player_X2 <- NULL
matchData$away_player_X3 <- NULL
matchData$away_player_X4 <- NULL
matchData$away_player_X5 <- NULL
matchData$away_player_X6 <- NULL
matchData$away_player_X7 <- NULL
matchData$away_player_X8 <- NULL
matchData$away_player_X9 <- NULL
matchData$away_player_X10 <- NULL
matchData$away_player_X11 <- NULL
matchData$away_player_Y1 <- NULL
matchData$away_player_Y2 <- NULL
matchData$away_player_Y3 <- NULL
matchData$away_player_Y4 <- NULL
matchData$away_player_Y5 <- NULL
matchData$away_player_Y6 <- NULL
matchData$away_player_Y7 <- NULL
matchData$away_player_Y8 <- NULL
matchData$away_player_Y9 <- NULL
matchData$away_player_Y10 <- NULL
matchData$away_player_Y11 <- NULL
matchData$home_player_1 <- NULL
matchData$home_player_2 <- NULL
matchData$home_player_3 <- NULL
matchData$home_player_4 <- NULL
matchData$home_player_5 <- NULL
matchData$home_player_6 <- NULL
matchData$home_player_7 <- NULL
matchData$home_player_8 <- NULL
matchData$home_player_9 <- NULL
matchData$home_player_10 <- NULL
matchData$home_player_11 <- NULL
matchData$home_player_12 <- NULL
matchData$away_player_1 <- NULL
matchData$away_player_2 <- NULL
matchData$away_player_3 <- NULL
matchData$away_player_4 <- NULL
matchData$away_player_5 <- NULL
matchData$away_player_6 <- NULL
matchData$away_player_7 <- NULL
matchData$away_player_8 <- NULL
matchData$away_player_9 <- NULL
matchData$away_player_10 <- NULL
matchData$away_player_11 <- NULL
matchData$away_player_12 <- NULL

matchData$goal <- NULL
matchData$shoton <- NULL
matchData$shotoff <- NULL
matchData$foulcommit <- NULL
matchData$card <- NULL
matchData$cross <- NULL
matchData$corner <- NULL
matchData$possession <- NULL



matchData$winner <- ifelse(matchData$home_team_goal == matchData$away_team_goal, 
                           "Draw", 
                           ifelse(matchData$home_team_goal > matchData$away_team_goal, 
                                  "Home Win",
                                  "Away Win"))
matchData$country_id..119 <- NULL
matchData$id..116 <- NULL
matchData$id..118 <- NULL
matchData$name <- NULL
matchData$id <- NULL
matchData$country_id <- NULL
matchData$league_id <- NULL
names(matchData)

#######################
# B365
#######################

#Additive
B365 <- data.frame(matchData$B365H,matchData$B365D, matchData$B365A)
B365 <- cbind(B365, implied_probabilities(B365, method="basic")$probabilities)
names(B365)[4] <- "addB365H"
names(B365)[5] <- "addB365D"
names(B365)[6] <- "addB365A"

#Shin (The method ’shin’ uses the method by Shin (1992, 1993). This model assumes that there is a
#fraction of insider trading, and that the bookmakers tries to maximize their profits. In addition to
#providing implied probabilties, the method also gives an estimate of the proportion if inside trade,
#denoted z. Two algorithms are implemented for finding the probabilities and z. Which algorithm to
#use is chosen via the shin_mehod argument. The default method (shin_method = ’js’) is based on
#the algorithm in Jullien & Salanié (1994). The ’uniroot’ method uses R’s built in equation solver
#to find the probabilities)
B365 <- cbind(B365, implied_probabilities(B365[1:3], method="shin")$probabilities)
names(B365)[7] <- "shinB365H"
names(B365)[8] <- "shinB365D"
names(B365)[9] <- "shinB365A"
#Cumulative (Weights Proportional to the Odds)
B365 <- cbind(B365, implied_probabilities(B365[1:3], method="wpo")$probabilities)
names(B365)[10] <- "cumB365H"
names(B365)[11] <- "cumB365D"
names(B365)[12] <- "cumB365A"
#Power
B365 <- cbind(B365, implied_probabilities(B365[1:3], method="power")$probabilities)
names(B365)[13] <- "powerB365H"
names(B365)[14] <- "powerB365D"
names(B365)[15] <- "powerB365A"

#other methods
#(The ’bb’ (short for "balanced books") method is from Fingleton & Waldron (1999), and is a variant
#  of Shin’s method. It too assume a fraction of insiders, but instead of assuming that the bookmakers
#  maximize their profits, they minimize their risk.)

names(B365)[1] <- "B365H"
names(B365)[2] <- "B365D"
names(B365)[3] <- "B365A"


#######################
# BW
#######################
#Additive
BW <- data.frame(matchData$BWH,matchData$BWD, matchData$BWA)
BW <- cbind(BW, implied_probabilities(BW, method="basic")$probabilities)
names(BW)[4] <- "addBWH"
names(BW)[5] <- "addBWD"
names(BW)[6] <- "addBWA"

BW <- cbind(BW, implied_probabilities(BW[1:3], method="shin")$probabilities)
names(BW)[7] <- "shinBWH"
names(BW)[8] <- "shinBWD"
names(BW)[9] <- "shinBWA"
BW <- cbind(BW, implied_probabilities(BW[1:3], method="wpo")$probabilities)
names(BW)[10] <- "cumBWH"
names(BW)[11] <- "cumBWD"
names(BW)[12] <- "cumBWA"
BW <- cbind(BW, implied_probabilities(BW[1:3], method="power")$probabilities)
names(BW)[13] <- "powerBWH"
names(BW)[14] <- "powerBWD"
names(BW)[15] <- "powerBWA"
names(BW)[1] <- "BWH"
names(BW)[2] <- "BWD"
names(BW)[3] <- "BWA"
#######################
# IW
#######################
IW <- data.frame(matchData$IWH,matchData$IWD, matchData$IWA)
IW <- cbind(IW, implied_probabilities(IW, method="basic")$probabilities)
names(IW)[4] <- "addIWH"
names(IW)[5] <- "addIWD"
names(IW)[6] <- "addIWA"

IW <- cbind(IW, implied_probabilities(IW[1:3], method="shin")$probabilities)
names(IW)[7] <- "shinIWH"
names(IW)[8] <- "shinIWD"
names(IW)[9] <- "shinIWA"
IW <- cbind(IW, implied_probabilities(IW[1:3], method="wpo")$probabilities)
names(IW)[10] <- "cumIWH"
names(IW)[11] <- "cumIWD"
names(IW)[12] <- "cumIWA"
IW <- cbind(IW, implied_probabilities(IW[1:3], method="power")$probabilities)
names(IW)[13] <- "powerIWH"
names(IW)[14] <- "powerIWD"
names(IW)[15] <- "powerIWA"
names(IW)[1] <- "IWH"
names(IW)[2] <- "IWD"
names(IW)[3] <- "IWA"

#######################
# LB
#######################
LB <- data.frame(matchData$LBH,matchData$LBD, matchData$LBA)
LB <- cbind(LB, implied_probabilities(LB, method="basic")$probabilities)
names(LB)[4] <- "addLBH"
names(LB)[5] <- "addLBD"
names(LB)[6] <- "addLBA"

LB <- cbind(LB, implied_probabilities(LB[1:3], method="shin")$probabilities)
names(LB)[7] <- "shinLBH"
names(LB)[8] <- "shinLBD"
names(LB)[9] <- "shinLBA"
LB <- cbind(LB, implied_probabilities(LB[1:3], method="wpo")$probabilities)
names(LB)[10] <- "cumLBH"
names(LB)[11] <- "cumLBD"
names(LB)[12] <- "cumLBA"
LB <- cbind(LB, implied_probabilities(LB[1:3], method="power")$probabilities)
names(LB)[13] <- "powerLBH"
names(LB)[14] <- "powerLBD"
names(LB)[15] <- "powerLBA"
names(LB)[1] <- "LBH"
names(LB)[2] <- "LBD"
names(LB)[3] <- "LBA"

#######################
# PS
#######################
PS <- data.frame(matchData$PSH,matchData$PSD, matchData$PSA)
PS <- cbind(PS, implied_probabilities(PS, method="basic")$probabilities)
names(PS)[4] <- "addPSH"
names(PS)[5] <- "addPSD"
names(PS)[6] <- "addPSA"

PS <- cbind(PS, implied_probabilities(PS[1:3], method="shin")$probabilities)
names(PS)[7] <- "shinPSH"
names(PS)[8] <- "shinPSD"
names(PS)[9] <- "shinPSA"
PS <- cbind(PS, implied_probabilities(PS[1:3], method="wpo")$probabilities)
names(PS)[10] <- "cumPSH"
names(PS)[11] <- "cumPSD"
names(PS)[12] <- "cumPSA"
PS <- cbind(PS, implied_probabilities(PS[1:3], method="power")$probabilities)
names(PS)[13] <- "powerPSH"
names(PS)[14] <- "powerPSD"
names(PS)[15] <- "powerPSA"
names(PS)[1] <- "PSH"
names(PS)[2] <- "PSD"
names(PS)[3] <- "PSA"

#######################
# WH
#######################
WH <- data.frame(matchData$WHH,matchData$WHD, matchData$WHA)
WH <- cbind(WH, implied_probabilities(WH, method="basic")$probabilities)
names(WH)[4] <- "addWHH"
names(WH)[5] <- "addWHD"
names(WH)[6] <- "addWHA"

WH <- cbind(WH, implied_probabilities(WH[1:3], method="shin")$probabilities)
names(WH)[7] <- "shinWHH"
names(WH)[8] <- "shinWHD"
names(WH)[9] <- "shinWHA"
WH <- cbind(WH, implied_probabilities(WH[1:3], method="wpo")$probabilities)
names(WH)[10] <- "cumWHH"
names(WH)[11] <- "cumWHD"
names(WH)[12] <- "cumWHA"
WH <- cbind(WH, implied_probabilities(WH[1:3], method="power")$probabilities)
names(WH)[13] <- "powerWHH"
names(WH)[14] <- "powerWHD"
names(WH)[15] <- "powerWHA"
names(WH)[1] <- "WHH"
names(WH)[2] <- "WHD"
names(WH)[3] <- "WHA"
#######################
# SJ
#######################
SJ <- data.frame(matchData$SJH,matchData$SJD, matchData$SJA)
SJ <- cbind(SJ, implied_probabilities(SJ, method="basic")$probabilities)
names(SJ)[4] <- "addSJH"
names(SJ)[5] <- "addSJD"
names(SJ)[6] <- "addSJA"

SJ <- cbind(SJ, implied_probabilities(SJ[1:3], method="shin")$probabilities)
names(SJ)[7] <- "shinSJH"
names(SJ)[8] <- "shinSJD"
names(SJ)[9] <- "shinSJA"
SJ <- cbind(SJ, implied_probabilities(SJ[1:3], method="wpo")$probabilities)
names(SJ)[10] <- "cumSJH"
names(SJ)[11] <- "cumSJD"
names(SJ)[12] <- "cumSJA"
SJ <- cbind(SJ, implied_probabilities(SJ[1:3], method="power")$probabilities)
names(SJ)[13] <- "powerSJH"
names(SJ)[14] <- "powerSJD"
names(SJ)[15] <- "powerSJA"
names(SJ)[1] <- "SJH"
names(SJ)[2] <- "SJD"
names(SJ)[3] <- "SJA"
#######################
# VC
#######################
VC <- data.frame(matchData$VCH,matchData$VCD, matchData$VCA)
VC <- cbind(VC, implied_probabilities(VC, method="basic")$probabilities)
names(VC)[4] <- "addVCH"
names(VC)[5] <- "addVCD"
names(VC)[6] <- "addVCA"

VC <- cbind(VC, implied_probabilities(VC[1:3], method="shin")$probabilities)
names(VC)[7] <- "shinVCH"
names(VC)[8] <- "shinVCD"
names(VC)[9] <- "shinVCA"
VC <- cbind(VC, implied_probabilities(VC[1:3], method="wpo")$probabilities)
names(VC)[10] <- "cumVCH"
names(VC)[11] <- "cumVCD"
names(VC)[12] <- "cumVCA"
VC <- cbind(VC, implied_probabilities(VC[1:3], method="power")$probabilities)
names(VC)[13] <- "powerVCH"
names(VC)[14] <- "powerVCD"
names(VC)[15] <- "powerVCA"
names(VC)[1] <- "VCH"
names(VC)[2] <- "VCD"
names(VC)[3] <- "VCA"
#######################
# GB
#######################
GB <- data.frame(matchData$GBH,matchData$GBD, matchData$GBA)
GB <- cbind(GB, implied_probabilities(GB, method="basic")$probabilities)
names(GB)[4] <- "addGBH"
names(GB)[5] <- "addGBD"
names(GB)[6] <- "addGBA"

GB <- cbind(GB, implied_probabilities(GB[1:3], method="shin")$probabilities)
names(GB)[7] <- "shinGBH"
names(GB)[8] <- "shinGBD"
names(GB)[9] <- "shinGBA"
GB <- cbind(GB, implied_probabilities(GB[1:3], method="wpo")$probabilities)
names(GB)[10] <- "cumGBH"
names(GB)[11] <- "cumGBD"
names(GB)[12] <- "cumGBA"
GB <- cbind(GB, implied_probabilities(GB[1:3], method="power")$probabilities)
names(GB)[13] <- "powerGBH"
names(GB)[14] <- "powerGBD"
names(GB)[15] <- "powerGBA"
names(GB)[1] <- "GBH"
names(GB)[2] <- "GBD"
names(GB)[3] <- "GBA"
#######################
# BSH
#######################
BS <- data.frame(matchData$BSH,matchData$BSD, matchData$BSA)
BS<- cbind(BS, implied_probabilities(BS, method="basic")$probabilities)
names(BS)[4] <- "addBSH"
names(BS)[5] <- "addBSD"
names(BS)[6] <- "addBSA"

BS <- cbind(BS, implied_probabilities(BS[1:3], method="shin")$probabilities)
names(BS)[7] <- "shinBSH"
names(BS)[8] <- "shinBSD"
names(BS)[9] <- "shinBSA"
BS <- cbind(BS, implied_probabilities(BS[1:3], method="wpo")$probabilities)
names(BS)[10] <- "cumBSH"
names(BS)[11] <- "cumBSD"
names(BS)[12] <- "cumBSA"
BS <- cbind(BS, implied_probabilities(BS[1:3], method="power")$probabilities)
names(BS)[13] <- "powerBSH"
names(BS)[14] <- "powerBSD"
names(BS)[15] <- "powerBSA"
names(BS)[1] <- "BSH"
names(BS)[2] <- "BSD"
names(BS)[3] <- "BSA"


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










