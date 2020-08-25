#######################
# Exploration
#######################
readRDS(file = "odds.rds")

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

missingsHome <-   cbind(matchData[1:9], 
                    matchData[12], 
                    matchData[15], 
                    matchData[18], 
                    matchData[21], 
                    matchData[24], 
                    matchData[27], 
                    matchData[30], 
                    matchData[33],
                    matchData[36],
                    matchData[40])

missingsDraw <-   cbind(matchData[1:7],
                        matchData[10],
                        matchData[13], 
                        matchData[16], 
                        matchData[19], 
                        matchData[22], 
                        matchData[25], 
                        matchData[28], 
                        matchData[31], 
                        matchData[34],
                        matchData[37],
                        matchData[40])

missingsAway <-    cbind(matchData[1:7],
                         matchData[11],
                         matchData[14], 
                         matchData[17], 
                         matchData[20], 
                         matchData[23], 
                         matchData[26], 
                         matchData[29], 
                         matchData[32], 
                         matchData[35],
                         matchData[38],
                         matchData[40])

#we can already see that we have missings, how is this for every betting house?
table(complete.cases(matchData[9:11]))


naniar::gg_miss_var(missingsHome[9:18]) +  labs(x="Betting houses", 
                                                y="Number of Missings", 
                                                title="Number of missings across our Betting houses")
naniar::gg_miss_upset(missingsHome[9:18])
naniar::gg_miss_upset(missingsHome, nsets = 26)
missingsTable <- naniar::miss_var_summary(missingsHome)
missingsTable
boxplot(missingsHome[9:18]) #we can make it better.


#with home
missingsHome[9:18] %>%
  pivot_longer(everything(), names_to = "BettingHouses",values_to = "values") %>%
  ggplot(aes(x = BettingHouses, y = values, fill = BettingHouses, color = BettingHouses))+
  geom_boxplot(alpha = 0.2) +
  ggtitle("How do betting odds distributed among brands when the local wins?") +
  theme(legend.position = "none")

missingsHome[9:18] %>%
  pivot_longer(everything(), names_to = "BettingHouses",values_to = "values") %>%
  filter(!is.na(values)) %>%
  ggplot(aes(x = BettingHouses, y = values, fill = BettingHouses, color = BettingHouses))+
  geom_boxplot(alpha = 0.2)


#draw
missingsDraw[8:17] %>%
  pivot_longer(everything(), names_to = "BettingHouses",values_to = "values") %>%
  ggplot(aes(x = BettingHouses, y = values, fill = BettingHouses, color = BettingHouses))+
  geom_boxplot(alpha = 0.2) +
  ggtitle("How do betting odds distributed among brands when there is a draw?") +
  theme(legend.position = "none")

#away
missingsAway[8:17] %>%
  pivot_longer(everything(), names_to = "BettingHouses",values_to = "values") %>%
  ggplot(aes(x = BettingHouses, y = values, fill = BettingHouses, color = BettingHouses))+
  geom_boxplot(alpha = 0.2) +
  ggtitle("How do betting odds distributed among brands when away wins?") +
  theme(legend.position = "none")

#with all
matchData[9:38] %>%
  pivot_longer(everything(), names_to = "BettingHouses",values_to = "values") %>%
  ggplot(aes(x = BettingHouses, y = values, fill = BettingHouses, color = BettingHouses))+
  geom_boxplot(alpha = 0.2) +
  ggtitle("How do betting odds distributed are distributed across brands?") +
  theme(legend.position = "none")


#So in what do the odds differ?

#filtering in the case of win:
missingsHome[9:19] %>%
  filter(missingsHome[19]=="Home Win") %>%
  select(everything(), -winner) %>%
  pivot_longer(everything(), names_to = "var",values_to = "values") %>%
  ggplot(aes(x = var, y = values, fill = var, color = var))+
  geom_boxplot(alpha = 0.2)

#filtering in the case of draw:
missingsDraw[8:18] %>%
  filter(missingsDraw[18]=="Draw") %>%
  select(everything(), -winner) %>%
  pivot_longer(everything(), names_to = "var",values_to = "values") %>%
  ggplot(aes(x = var, y = values, fill = var, color = var))+
  geom_boxplot(alpha = 0.2)

#filtering in the case of away:
missingsAway[8:18] %>%
  filter(missingsAway[18]=="Away Win") %>%
  select(everything(), -winner) %>%
  pivot_longer(everything(), names_to = "var",values_to = "values") %>%
  ggplot(aes(x = var, y = values, fill = var, color = var))+
  geom_boxplot(alpha = 0.2)


#which odd is the one that each house proposed when was correct?
filteredMissingsHome2 <- missingsHome[9:19] %>%
  filter(missingsHome[19]=="Home Win") %>%
  select(everything(), -winner)
#we can see here the info:
summary(filteredMissingsHome2)

filteredMissingsDraw2 <- missingsDraw[8:18] %>%
  filter(missingsDraw[18]=="Draw") %>%
  select(everything(), -winner) 
#we can see here the info:
summary(filteredMissingsDraw2)

filteredMissingsAway2 <- missingsAway[8:18] %>%
  filter(missingsAway[18]=="Away Win") %>%
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

filteredMissingsDraw21 <- missingsDraw[8:18] %>%
  filter(missingsDraw[18]!="Draw") %>%
  select(everything(), -winner) 
#we can see here the info:
summary(filteredMissingsDraw21)

filteredMissingsAway21 <- missingsAway[8:18] %>%
  filter(missingsAway[18]!="Away Win") %>%
  select(everything(), -winner)
#we can see here the info:
summary(filteredMissingsAway21)

##############################
# Outliers and correcting NA's
##############################
#ok, now we've explored a bit our data. What we decide to remove? What is an outlier?
#we imputate NA's? etc.


#we have this many missings:
missingsTable




