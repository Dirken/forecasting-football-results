#######################
# Exploration
#######################
readRDS(file = "odds.rds")

missingsAway <- data.frame(season = matchData$season,
                      stage = matchData$stage,
                      date = matchData$date,
                      match_api_id = matchData$match_api_id,
                      home_team_api_id = matchData$home_team_api_id,
                      away_team_api_id = matchData$away_team_api_id,
                      home_team_goal = matchData$home_team_goal,
                      away_team_goal = matchData$away_team_goal,
                      B365A = matchData$B365A,
                      BWA = matchData$BWA,
                      IWA = matchData$IWA,
                      LBA = matchData$LBA,
                      PSA = matchData$PSA,
                      WHA = matchData$WHA,
                      SJA = matchData$SJA,
                      VCA = matchData$VCA,
                      GBA = matchData$GBA,
                      BSA = matchData$BSA,
                      winner = matchData$winner)

missingsDraw <-   data.frame(season = matchData$season,
                             stage = matchData$stage,
                             date = matchData$date,
                             match_api_id = matchData$match_api_id,
                             home_team_api_id = matchData$home_team_api_id,
                             away_team_api_id = matchData$away_team_api_id,
                             home_team_goal = matchData$home_team_goal,
                             away_team_goal = matchData$away_team_goal,
                             B365D = matchData$B365D,
                             BWD = matchData$BWD,
                             IWD = matchData$IWD,
                             LBD = matchData$LBD,
                             PSD = matchData$PSD,
                             WHD = matchData$WHD,
                             SJD = matchData$SJD,
                             VCD = matchData$VCD,
                             GBD = matchData$GBD,
                             BSD = matchData$BSD,
                             winner = matchData$winner)

missingsHome <-    data.frame(season = matchData$season,
                              stage = matchData$stage,
                              date = matchData$date,
                              match_api_id = matchData$match_api_id,
                              home_team_api_id = matchData$home_team_api_id,
                              away_team_api_id = matchData$away_team_api_id,
                              home_team_goal = matchData$home_team_goal,
                              away_team_goal = matchData$away_team_goal,
                              B365H = matchData$B365H,
                              BWH = matchData$BWH,
                              IWH = matchData$IWH,
                              LBH = matchData$LBH,
                              PSH = matchData$PSH,
                              WHH = matchData$WHH,
                              SJH = matchData$SJH,
                              VCH = matchData$VCH,
                              GBH = matchData$GBH,
                              BSH = matchData$BSH,
                              winner = matchData$winner)

#we can already see that we have missings, how is this for every betting house?
table(complete.cases(matchData[10:12]))


naniar::gg_miss_var(missingsHome[9:18]) +  labs(x="Betting houses", 
                                                y="Number of Missings", 
                                                title="Number of missings across our Betting houses")
naniar::gg_miss_upset(missingsHome[9:18])
naniar::gg_miss_upset(missingsHome, nsets = 26)
missingsTable <- naniar::miss_var_summary(missingsHome)


boxplotLeagues <- function(dataset, title){
  dataset %>%
    pivot_longer(everything(), names_to = "BettingHouses",values_to = "values") %>%
    ggplot(aes(x = BettingHouses, y = values, fill = BettingHouses, color = BettingHouses))+
    geom_boxplot(alpha = 0.2) +
    ggtitle(title) +
    theme(legend.position = "none")
}

boxplotLeagues(missingsHome[9:18],"How do betting odds distributed among brands when the local wins?")
boxplotLeagues(missingsDraw[9:18],"How do betting odds distributed among brands when there is a draw?")
boxplotLeagues(missingsAway[9:18],"How do betting odds distributed among brands when away wins?")
boxplotLeagues(matchData[9:38],"How do betting odds distributed are distributed across brands?")

boxplotLeaguesFiltered <- function(dataset, filterKeyword){
  dataset %>%
    filter(dataset[,ncol(dataset)]=="Home Win") %>%
    select(everything(), -winner) %>%
    pivot_longer(everything(), names_to = "var",values_to = "values") %>%
    ggplot(aes(x = var, y = values, fill = var, color = var))+
    geom_boxplot(alpha = 0.2)
}

boxplotLeaguesFiltered(missingsHome[9:19], "Home Win")
boxplotLeaguesFiltered(missingsDraw[9:19], "Draw")
boxplotLeaguesFiltered(missingsAway[9:19], "Away Win")

#which odd is the one that each house proposed when was correct?
filteredMissingsHome2 <- missingsHome[9:19] %>%
  filter(missingsHome[19]=="Home Win") %>%
  select(everything(), -winner)
#we can see here the info:
summary(filteredMissingsHome2)

filteredMissingsDraw2 <- missingsDraw[9:19] %>%
  filter(missingsDraw[19]=="Draw") %>%
  select(everything(), -winner) 
#we can see here the info:
summary(filteredMissingsDraw2)

filteredMissingsAway2 <- missingsAway[9:19] %>%
  filter(missingsAway[19]=="Away Win") %>%
  select(everything(), -winner)
#we can see here the info:
summary(filteredMissingsAway2)

#unique(missingsAway$home_team_api_id[missingsAway$B365A == 34 & missingsAway$BWA == 21])
#8634 8633
#barça                      madrid
#Hércules Club de Fútbol    eibar
#"2010-09-11 00:00:00" "2015-04-11 00:00:00"


#which odd is the one that each house proposed when was NOT correct?
filteredMissingsHome21 <- missingsHome[9:19] %>%
  filter(missingsHome[19]!="Home Win") %>%
  select(everything(), -winner)
#we can see here the info:
summary(filteredMissingsHome21)

filteredMissingsDraw21 <- missingsDraw[9:19] %>%
  filter(missingsDraw[19]!="Draw") %>%
  select(everything(), -winner) 
#we can see here the info:
summary(filteredMissingsDraw21)

filteredMissingsAway21 <- missingsAway[9:19] %>%
  filter(missingsAway[19]!="Away Win") %>%
  select(everything(), -winner)
#we can see here the info:
summary(filteredMissingsAway21)

##############################
# Outliers and correcting NA's
##############################
#ok, now we've explored a bit our data. What we decide to remove? What is an outlier?
#we imputate NA's? etc.

results



