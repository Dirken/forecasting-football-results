#######################
#Card
#######################
cards  <- incidents %>% filter(type == 'card')

#do we have locations on here?
#unique(cards$lon) --> NO, WE DON'T

cards$lon <- NULL
cards$lat <- NULL
cards$type <- NULL  #Obviously i already know since i filter.
cards$player2 <- NULL #Always NULL, only one player recevies the card ;)

names(cards)

table(cards$subtype1)
#############
#Cards by minute split by part
#############
cards %>%
  ggplot(mapping = aes(x = half_elapsed)) +
  geom_bar() +
  facet_wrap(~half, scales = "free_x")

table(cards$half_elapsed)
#############
#Cards+type by minute split by part, barplot
#############

#%realment estan be aquests half_elapsed? ho hauria de haver un elapsed_plus?

cards %>%
  ggplot(mapping = aes(x = half_elapsed)) +
  geom_bar(mapping = aes(fill = subtype1)) +
  scale_color_viridis(discrete = T) +
  facet_wrap(~half, scales = "free_x") +
  theme(legend.direction ="horizontal", legend.position = "bottom")

cards %>%
  ggplot(mapping = aes(x = half_elapsed)) +
  geom_bar(mapping = aes(fill = subtype1)) +
  scale_color_viridis(discrete = T) +
  facet_wrap(~subtype2, scales = "free_x")
  


View(cards)

#############
#Percentages
#############

#How many per match???
percentages1 <- table(cards$subtype1)
percentages1 #many yellow cards, how many per game?
yellowCardsAvg <- percentages1[['y']]/length(unique(cards$game_id)) #4.213487
secondYellow <- percentages1[['y2']]/length(unique(cards$game_id)) #0.1239837
redCardsAvg <- percentages1[['r']]/length(unique(cards$game_id)) #0.1274681

#Percentually, how each of this events occurr
foulsPercentages1 <- lapply(percentages1, function(x){100*x/sum(percentages1)})
foulsPercentages1
percentages2 <- table(cards$subtype2) #i don't see here NA's...
foulsPercentages2 <- lapply(percentages, function(x){100*x/sum(percentages)})

#############
#Type of cards per part
#############

half <- cards %>%
  select(c(subtype1, subtype2,half))
percent <- table(half$subtype1, half$half)
100*percent[,1] / (percent[,2] + percent[,1])
100*percent[,2] / (percent[,2] + percent[,1])

#way more cards in second half always

#############
#From those actions, which is the action that brings the mosts expulsions?
#############
yOnly <- cards %>%
  filter(subtype1 == "y")

y2Only <- cards %>%
  filter(subtype1 == "y2")
redCardsOnly <- cards %>%
  filter(subtype1 == "r")

par(las=2)
par(mar=c(10,4,4,2))
#we can see differences in between each of the actions taken from the referee
barplot(c(396 , 435 , 1179 , 472 , 252 , 520 , 154 , 7145 , 721 , 634 , 3267 , 1898 , 250), 
        names.arg = c("advantage" , "diving" , "emergency_brake" , "hands" ,
                      "kicked_ball_away" , "pushing" , "Removing Shirt" , "serious_fouls" , 
                      "shirt_pull" , "stall_time" , "Unsportsmanlike Cond" , "verbal_abuse" , 
                      "violence"))
table(y2Only$subtype2)

barplot(c(12 , 39 , 38 , 11 , 16 , 1 , 276 , 9 , 3 , 44, 33 , 15), 
        names.arg = c("diving" , "emergency_brake" , "hands" ,
                      "kicked_ball_away" , "pushing" , "Removing Shirt" , "serious_fouls" , 
                      "shirt_pull" , "stall_time" , "Unsportsmanlike Cond" , "verbal_abuse" , 
                      "violence"))

table(redCardsOnly$subtype2)
barplot(c(2 , 93 , 19 , 5 , 245 , 20 , 112 , 59 , 129), 
        names.arg = c("diving" , "emergency_brake" , "hands" ,
                      "pushing" , "serious_fouls" , 
                      "shirt_pull" , "Unsportsmanlike Cond" , "verbal_abuse" , 
                      "violence"))

#############
#Are there leagues with more cards than others? linking team with its league...
#############

#############
#Are there teams with more cards than others? Do they lose more? Dirty teams lose?
#############
teams <- NULL
teams <- cards %>%
  select(team)
length(table(teams)) #only 202 teams

barplot(head(sort(table(teams), decreasing = T),15))

indexes <- names(head(sort(table(teams), decreasing = T),50))
top50CardsTeams <- NULL
for(i in indexes){
  top50CardsTeams <- paste(top50CardsTeams, print(team$team_long_name[team$team_api_id == i]) , sep=",")
}

print(team$team_long_name[team$team_api_id == "8558"])
top50CardsTeams <- strsplit(top50CardsTeams, split=",")

headRes <- head(sort(table(teams), decreasing=T),15)
barplot(headRes, 
        names.arg = head(top50CardsTeams[[1]],16)[-1],
        ylim = c(0,1000))

#here we can see that we only have some leagues -reminder----
#######################
#How related are cards and fouls commited? 
#######################

foulCommit  <- incidents %>% filter(type == 'foulcommit')

unique(foulCommit$subtype2)
foulCommit$subtype2 <- NULL 
data <- unique(foulCommit$subtype1)
data
unique(cards$subtype2)

#######################
#lets see how fouls distribute
#######################
foulCommit %>%
  ggplot(mapping = aes(x = lon, y = lat)) +
  geom_jitter(mapping = aes(colour = subtype1), alpha = 0.3, size = 2, stroke = 0) +
  scale_color_viridis(discrete = T) +
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  facet_wrap(~half) +
  theme_minimal() +
  theme(legend.direction ="horizontal", legend.position = "bottom")

foulCommit <- foulCommit[!is.na(foulCommit)]
#All na's? what? I'll just remove the factor so the plot doesn't complain

data <- unique(data)[-1]

for(i in data){
  fileName <- paste0("./factorsImages/fouls/",i, ".png")
  png(filename = fileName, bg="transparent")
  ggPlot <- foulCommit %>%
    filter(subtype1 == i) %>%
    ggplot(mapping = aes(x = lon, y = lat)) +
    geom_jitter(mapping = aes(colour = subtype1), alpha = 0.3, size = 2, stroke = 0) +
    scale_color_viridis(discrete = T) +
    guides(colour = guide_legend(override.aes = list(alpha = 1))) +
    facet_wrap(~half) +
    theme_minimal()
  print(ggPlot)
  dev.off()
}

#######################
#Objective: Merging both
#######################

names(foulCommit)[4]
colnames(foulCommit)[3] <- "subtypeFoul2"
colnames(foulCommit)[4] <- "subtypeFoul1"

total <- NULL
total <- merge(foulCommit, cards, by = c("game_id", "elapsed", "elapsed_plus", "half", "half_elapsed"), all.x=TRUE)
total

total %>%
  filter(subtype1 == 'y') %>%
  ggplot(mapping = aes(x = lon, y = lat)) +
  geom_jitter(mapping = aes(colour = subtypeFoul2), alpha = 0.3, size = 2, stroke = 0) +
  scale_color_viridis(discrete = T) +  
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  facet_wrap(~half) +
  theme_minimal() +
  theme(legend.direction ="horizontal", legend.position = "bottom")

pitch + total %>%
  filter(subtype1 == 'y2') %>%
  geom_jitter(mapping = aes(colour = subtypeFoul2), alpha = 1.0, size = 2, stroke = 0) +
  scale_color_viridis(discrete = T) +  
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  theme(legend.direction ="horizontal", legend.position = "bottom") +
  scale_color_brewer(palette="Set3")


pitch + total %>%
  filter(subtype1 == 'r') %>%
  geom_jitter(mapping = aes(colour = subtypeFoul2), alpha = 1.0, size = 2, stroke = 0) +
  scale_color_viridis(discrete = T) +  
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  theme(legend.direction ="horizontal", legend.position = "bottom") +
  scale_color_brewer(palette="Accent")

unique(total$id.x[total$subtype1 == "y" && is.null(total$lon.x)]) #what?
matrixplot(total)
yellowCardsMatches <- total %>%
  filter(subtype1 == 'y')
matrixplot(yellowCardsMatches)

twoYellowCardsMatches <- total %>%
  filter(subtype1 == 'y2')

redCardMatches <- total %>%
  filter(subtype1 == 'r')

redCardMatches <- cards %>%
  filter(subtype1 == 'r')

redCardMatch <- match %>% 
  filter(match$id %in% redCardMatches$game_id) #aixo no és així

redCardMatch <- redCardMatch %>%
  filter(redCardMatch$home_team_api_id %in% redCardMatches$team)

localFactorRedCard <- redCardMatch %>%
  select(home_team_goal, away_team_goal)

localFactorRedCard$result <- ifelse(localFactorRedCard$home_team_goal == localFactorRedCard$away_team_goal,
                                    'D', ifelse(localFactorRedCard$home_team_goal > localFactorRedCard$away_team_goal, 'H', 'A'))
#

table(localFactorRedCard$result)
#how many fouls get card?


#how does this affect winning / losing

#-take matches with red cards,
# see matches from those goals
#see what happens

redCards <- cards %>% filter(subtype1 %in% c('r','y2'))
redCards <- redCards %>% select(subtype1,game_id, half_elapsed)
names(redCards)[1] <- "type"

filteredGoals <- incidents %>% select(game_id, half_elapsed, type) %>% filter(type == "goal")

goalsAndCards <- merge(filteredGoals, redCards, all=T)
#removing matches which only have goals :)
write.csv2(goalsAndCards, file="goalsAndCards.csv")

indexGames <- c(1964, 2246, 2252, 2599, 3255, 3725, 3864, 3874, 3926, 4189, 4342, 4412, 4424, 4618, 5636, 5682, 5725, 5793, 6005, 6186, 6289, 6302, 6439, 6538, 6631, 6744, 6856, 6872, 6921, 7147, 7178, 8039, 8081, 8341, 8542, 8617, 8682, 8714, 8823, 8835, 9058, 9235, 9239, 9353, 9391, 9394, 9541, 9555, 9569, 9618, 9627, 9628, 9735, 9832, 10108, 10296, 10329, 10565, 10588, 10607, 10772, 10842, 10860, 10920, 10948, 10973, 10999, 11134, 11207, 11215, 11236, 11276, 11606, 11629, 11679, 11706, 11721, 11731, 11797, 11816, 11853, 11866, 11873, 11900, 11909, 11935, 11962, 12020, 12030, 12062, 12082, 12084, 12162, 12249, 12265, 12428, 12462, 12492, 12712, 12824, 12966, 12974, 13143, 13247, 15580, 15681, 21562, 21654, 21730, 21753, 21823, 21856, 21922, 21931, 21932, 21940, 22012, 22035, 22083, 22101, 22159, 22364, 22603, 22659, 22663, 22671, 22696, 22713, 22744, 22780, 22794, 22861, 22867, 22969, 23006, 23011, 23051, 23117, 23154, 23172, 23175, 23196, 23280, 23576, 23688, 23762, 23986, 24127, 24150, 24418, 24557, 25892)

goalsAndCards <- goalsAndCards %>% select(game_id, half_elapsed, type) %>% filter(game_id %in% indexGames)

redCardsMinutes <-  goalsAndCards %>% select(game_id, half_elapsed, type) %>% filter(!type == 'goal')
#65.96853


goalsAndCardsWhereThereIsCard <- filter(goalsAndCards, game_id %in% redCardsMinutes$game_id)
goalsAndCardsWhereThereIsCard %>% group_by(game_id)%>% having(time > 90) %>% count()

print(redCardMinutes)

