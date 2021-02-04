#######################
#Goal
#######################
goals  <- incidents %>% filter(type == 'goal')

table(goals$subtype1)
table(goals$subtype2)

png(filename = "./factorsImages/goals/goalsLat.png")
goalsLat <- goals %>%
  ggplot(mapping = aes(x = lat)) +
  geom_bar(mapping = aes(fill = subtype1)) +
  scale_color_viridis(discrete = T)+
  ggtitle("Goals by latitude")
print(goalsLat)
dev.off()

png(filename = "./factorsImages/goals/goalsLon.png")
goalsLon <- goals %>%
  ggplot(mapping = aes(x = lon)) +
  geom_bar(mapping = aes(fill = subtype1)) +
  scale_color_viridis(discrete = T) +
  ggtitle("Goals by longitude")
print(goalsLon)
dev.off()

#printing all variables in a map:

png(filename = "./factorsImages/goals/goalsAllSubtypes.png")
ggPlot <- pitch + goals %>%
  filter(! subtype1 == 'n' ) %>%
  geom_jitter(mapping = aes(colour = subtype1), alpha = 0.3, size = 2, stroke = 0) +
  #scale_color_viridis(discrete = T) +
  guides(colour = guide_legend(override.aes = list(alpha = 1))) + coord_flip()
print(ggPlot)
dev.off()


pitch + goals %>%
  geom_jitter(mapping = aes(colour = subtype1), alpha = 0.3, size = 2, stroke = 0) +
  scale_color_viridis(discrete = T) +
  guides(colour = guide_legend(override.aes = list(alpha = 1))) 

#printing one by one:
data <- unique(goals$subtype1)
data <- data[-8]
for(i in data){
  fileName <- paste0("./factorsImages/goals/",i, ".png")
  png(filename = fileName, , width = 1200, height = 860)
  ggPlot <- pitch + goals %>%
    filter(subtype1 == i) %>%
    geom_jitter(mapping = aes(colour = i), alpha = 0.5, size = 3, stroke = 0) +
    #scale_color_viridis(discrete = T) +
    guides(colour = guide_legend(override.aes = list(alpha = 1))) 
    #facet_wrap(~half) 
  print(ggPlot)
  dev.off()
}#we have to study what each subtype means....

narrowmatch  <- match %>%
  select(id, country_id, league_id, season, home_team_api_id, away_team_api_id, home_team_goal, away_team_goal)

goals  <- incidents %>% 
  filter(type == 'goal') %>% 
  inner_join(narrowmatch, by = c('game_id' = 'id'), copy = T)

library(dplyr)
library(magrittr)
goals %<>% mutate(scored_by = ifelse(team == home_team_api_id, 'home', 'away'))
goals %<>%
  mutate(lat = ifelse(half == 1, lat, 70 - lat),
         lon = ifelse(half == 1, lon, 46 - lon))

pitch.l = 105
pitch.w = 68

add_rectangle <- function(orig, miny, maxy, width) { 
  mid <- pitch.w / 2 
  halfwidth <- width / 2 
  rbind(orig,
        c(x1 = mid - halfwidth, y1 = miny, x2 = mid + halfwidth, y2 = miny),
        c(x1 = mid - halfwidth, y1 = miny, x2 = mid - halfwidth, y2 = maxy),
        c(x1 = mid - halfwidth, y1 = maxy, x2 = mid + halfwidth, y2 = maxy),
        c(x1 = mid + halfwidth, y1 = maxy, x2 = mid + halfwidth, y2 = miny)
  )
}

field <- tibble(x1 = 0, y1 = pitch.l / 2, x2 = pitch.w, y2 = pitch.l / 2) %>%
  add_rectangle(0, pitch.l, pitch.w) %>%
  add_rectangle(0, -2.44, 7.32) %>%
  add_rectangle(pitch.l, pitch.l + 2.44, 7.32) %>%
  add_rectangle(0, 5.5, 18.32) %>%
  add_rectangle(pitch.l, pitch.l - 5.5, 18.32) %>%
  add_rectangle(0, 16.5, 40.32) %>%
  add_rectangle(pitch.l, pitch.l - 16.5, 40.32) * (2/3)


penalty_curve_bottom  <- tibble(x1 = (pitch.w / 2) - 7.312, y1 = 16.5,
                                x2 = (pitch.w / 2) + 7.312, y2 = 16.5) * (2/3)
penalty_curve_top  <- tibble(x1 = (pitch.w / 2) - 7.312, y1 = pitch.l - 16.5,
                             x2 = (pitch.w / 2) + 7.312, y2 = pitch.l - 16.5) * (2/3)
corner_bottom <- tibble(x1 = c(0, pitch.w - 1), y1 = c(1, 0),
                        x2 = c(1, pitch.w)    , y2 = c(0, 1)) * (2/3)
corner_top <- tibble(x1 = c(0, pitch.w - 1), y1 = c(pitch.l -1, pitch.l    ),
                     x2 = c(1, pitch.w    ), y2 = c(pitch.l   , pitch.l - 1)) * (2/3)
circle  <- tibble(x1 = (pitch.w / 2) - 9.15, y1 = pitch.l / 2,
                  x2 = (pitch.w / 2) + 9.15, y2 = pitch.l / 2) * (2/3)


pitch.l = pitch.l * (2/3)
pitch.w = pitch.w * (2/3)
pitch.aes  <- aes(x = x1, y = y1, xend = x2, yend = y2)
stripes.aes <- aes(x = 10, xend=20)

pitch  <- ggplot(mapping = aes(x = lon, y = lat)) +
  field                   %>% geom_segment(mapping = pitch.aes, colour = 'white', size = 1.25) +
  circle                  %>% geom_curve(  mapping = pitch.aes, colour = 'white', curvature = 1, ncp = 20, size = 1.25) +
  circle                  %>% geom_curve(  mapping = pitch.aes, colour = 'white', curvature = -1, ncp = 20, size = 1.25) +
  penalty_curve_bottom    %>% geom_curve(  mapping = pitch.aes, colour = 'white', curvature = +0.4, size = 1.25) +
  penalty_curve_top       %>% geom_curve(  mapping = pitch.aes, colour = 'white', curvature = -0.4, size = 1.25) +
  corner_bottom           %>% geom_curve(  mapping = pitch.aes, colour = 'white', curvature = -0.5, size = 1.25) +
  corner_top              %>% geom_curve(  mapping = pitch.aes, colour = 'white', curvature = 0.5, size = 1.25) +
  coord_fixed((pitch.l/pitch.w)/1.546) +
  scale_x_continuous(breaks = 1:10 / 10 * pitch.w, labels = NULL) +
  scale_y_continuous(breaks = 1:22 / 22 * pitch.l, labels = NULL) +
  theme(panel.background = element_rect(fill = "#05890b",
                                        size = 1, linetype = "solid" ),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + coord_flip()

print(pitch)
#first half:

pitch + goals %>% 
  filter(half == 1) %>%
  geom_jitter(mapping = aes(colour = scored_by), alpha = 0.3, size = 2, stroke = 0) +
  scale_color_viridis(discrete = T) +
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  labs(title = "Scoring positions") 
#second half:
pitch +  goals %>% 
  filter(half == 2) %>%
  geom_jitter(mapping = aes(colour = scored_by), alpha = 0.3, size = 2, stroke = 0) +
  scale_color_viridis(discrete = T) +
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  labs(title = "Scoring positions") 
#both together :)
pitch + goals %>% 
  geom_jitter(mapping = aes(colour = scored_by), alpha = 0.3, size = 2, stroke = 0) +
  scale_color_viridis(discrete = T) +
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  labs(title = "Scoring positions") +
  facet_wrap(~half, labeller = label_both) 


#################################
#More goals in the first or second part?
#################################
firstHalfGoals <- goals %>% 
  filter(half == 1) %>%
  summarize(firstHalfGoals = n())


secondHalfGoals <- goals %>% 
  filter(half == 2) %>%
  summarize(secondHalfGoals = n())
#We can see that we have more goals in the first part than in the second, and quite significantly indeed.

percentage <- 100*firstHalfGoals/(firstHalfGoals+secondHalfGoals)
percentage
goals %>%
  ggplot(mapping = aes(x = half_elapsed)) +
  geom_bar(mapping = aes(fill = subtype1)) +
  scale_color_viridis(discrete = T) +
  facet_wrap(~half, scales = "free_x")


percentage

#################################
#How they distribute in Home/Away form?
#################################

goals %>%
  ggplot(mapping = aes(x = half_elapsed)) +
  geom_bar(mapping = aes(fill = scored_by)) +
  scale_color_viridis(discrete = T) 
#we can see that actually there are more goals from the home team as
#we knew from before (the average)

#if we split it on halfs:
goals %>%
  ggplot(mapping = aes(x = half_elapsed)) +
  geom_bar(position="dodge",mapping = aes(fill = scored_by)) +
  scale_color_viridis(discrete = T) +
  facet_wrap(~half, scales = "free_x")
#in any moment the visitor scores more than the local in average.


#############################################################
#How this contributes to win? Winning first part, helps win?
#############################################################

summaryPartsH <- goals %>%
  filter(half == 1) %>% 
  filter(scored_by == 'home') %>%
  group_by(season, country_id, home_team_api_id, away_team_api_id, game_id) %>%
  summarise(localGoals1stPart = n())

summaryPartsA <- goals %>%
  filter(half == 1) %>% 
  filter(scored_by == 'away') %>%
  group_by(season, country_id, home_team_api_id, away_team_api_id, game_id) %>%
  summarise(localGoals1stPart = n())

summaryPartsH2 <- goals %>%
  filter(half == 2) %>% 
  filter(scored_by == 'home') %>%
  group_by(season, country_id, home_team_api_id, away_team_api_id, game_id) %>%
  summarise(localGoals2ndPart = n())

summaryPartsA2 <- goals %>%
  filter(half == 2) %>% 
  filter(scored_by == 'away') %>%
  group_by(season, country_id, home_team_api_id, away_team_api_id, game_id) %>%
  summarise(localGoals2ndPart = n())


resFirstPart <- full_join(summaryPartsH, summaryPartsA,by=c("season", "country_id", "home_team_api_id", "away_team_api_id", "game_id"))

resFirstPart[is.na(resFirstPart)] <- 0

resFirstPart$winnerFirstPart <- ifelse(resFirstPart$localGoals1stPart.x > resFirstPart$localGoals1stPart.y, "H",
                                       ifelse(resFirstPart$localGoals1stPart.x == resFirstPart$localGoals1stPart.y, "D",
                                       "A"))

resSecondPart <- NULL
resSecondPart <- full_join(summaryPartsH2, summaryPartsA2,by=c("season", "country_id", "home_team_api_id", "away_team_api_id", "game_id"))
resSecondPart[is.na(resSecondPart)] <- 0

resSecondPart$winnerFirstPart <- ifelse(resSecondPart$localGoals2ndPart.x > resSecondPart$localGoals2ndPart.y, "H",
                                       ifelse(resSecondPart$localGoals2ndPart.x == resSecondPart$localGoals2ndPart.y, "D",
                                              "A"))

result <- full_join(resFirstPart, resSecondPart, by=c("season", "country_id", "home_team_api_id", "away_team_api_id", "game_id"))

result[is.na(result)] <- 0

result$totalMatchH <- result$localGoals1stPart.x + result$localGoals2ndPart.x

result$totalMatchA <- result$localGoals1stPart.y+ result$localGoals2ndPart.y 

result$finalRes <- ifelse(result$totalMatchH > result$totalMatchA, "H",
                          ifelse(result$totalMatchH == result$totalMatchA, "D",
                                 "A"))

DA <- result %>% filter(winnerFirstPart.x == 'D') %>% filter(finalRes == 'A')
length(DA$season) #440

DH <- result %>% filter(winnerFirstPart.x == 'D') %>% filter(finalRes == 'H')
length(DH$season) # 620

DD <- result %>% filter(winnerFirstPart.x == 'D') %>% filter(finalRes == 'D')
length(DD$season) #601

HD <- result %>% filter(winnerFirstPart.x == 'H') %>% filter(finalRes == 'D')
length(HD$season) #695

HA <- result %>% filter(winnerFirstPart.x == 'H') %>% filter(finalRes == 'A')
length(HA$season) #300

HH <- result %>% filter(winnerFirstPart.x == 'H') %>% filter(finalRes == 'H')
length(HH$season) #4073

AA <- result %>% filter(winnerFirstPart.x == 'A') %>% filter(finalRes == 'A')
length(AA$season) #2320

AD <- result %>% filter(winnerFirstPart.x == 'A') %>% filter(finalRes == 'D')
length(AD$season) #678

AH <- result %>% filter(winnerFirstPart.x == 'A') %>% filter(finalRes == 'H')
length(AH$season) #359

barPlotData <- NULL
barPlotData$values <- NULL
barPlotData$values <- c(440,620,601,695,300,4073,2320,678,359)
barPlotData$names <- c("DA","DH","DD","HD","HA","HH","AA","AD","AH")

a <- 4073 / sum(barPlotData$values)
a
b <- 2320 / sum(barPlotData$values)
b
100*(300 + 359) / sum(barPlotData$values)
barplot(barPlotData$values, names.arg = barPlotData$names)

changeL <- length(result$game_id[result$winnerFirstPart.x != result$finalRes])
all <- length(result$game_id)
100*changeL/all

#diff of goals? density plot?
mean(result$totalMatchH)
mean(result$totalMatchA)
mean(result$totalMatchH - result$totalMatchA)

result %>%
  ggplot( aes(x=result$totalMatchH, fill="#69b3a2")) +
  geom_density( color="#69b3a2", alpha=0.6) +
  labs(fill="")


result %>%
  ggplot( aes(x=result$totalMatchA, fill="#404080")) +
  geom_density( color="#404080", alpha=0.6) +
  labs(fill="")

result %>% 
  ggplot( aes(x) ) + 
  geom_density( aes(x = result$totalMatchH, y = ..density..), fill="red", alpha=0.6 ) + 
  #geom_label( aes(x=4.5, y=0.25, label="Home Densities"), color="#69b3a2") +
  geom_density( aes(x = result$totalMatchA, y = ..density..), fill= "green", alpha=0.6) +
  #geom_label( aes(x=4.5, y=-0.25, label="Away Densities"), color="#404080") +
  xlab("Goals per match") +
  geom_vline(xintercept =  mean(result$totalMatchA), size = 1, color ="green") +
  geom_vline(xintercept = mean(result$totalMatchH), size= 1, color="red") +
  theme(legend.direction ="horizontal", legend.position = "bottom")
  


#saveRDS(result, "./rds/result.rds")

