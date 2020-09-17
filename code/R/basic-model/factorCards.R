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
  facet_wrap(~half, scales = "free_x")

cards %>%
  ggplot(mapping = aes(x = half_elapsed)) +
  geom_bar(mapping = aes(fill = subtype1)) +
  scale_color_viridis(discrete = T) +
  facet_wrap(~subtype2, scales = "free_x")

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
table(half$subtype1, half$half)
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


#we can see differences in between each of the actions taken from the referee
table(yOnly$subtype2)
table(y2Only$subtype2)
table(redCardsOnly$subtype2)

#############
#Are there leagues with more cards than others? linking team with its league...
#############

#############
#Are there teams with more cards than others? Do they lose more? Dirty teams lose?
#############
teams <- cards %>%
  select(team)
sort(table(teams))

