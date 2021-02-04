library("ggplot2")
library("corrplot")

source("loadingDB.R")

#joining player and player_attrs.
player_attrs <-  player_attributes %>%
  rename(player_attrs_id = id) %>%
  left_join(player, by = "player_api_id")
player_attributes_q <- player_attrs [, c(3,44,4,5,6,10:42)]

#we take lat value for predict purposes
most_recent_p <- 
  player_attributes_q %>% 
  group_by(player_api_id) %>% 
  top_n(n = 1, wt = date) %>%
  as.data.frame()

top500 <- 
  most_recent_p %>% 
  arrange(desc(overall_rating)) %>% 
  head(n = 500) %>%
  as.data.frame()




row.names(top500) <-top500[,1]
player_biplot <- top500 [, c(2,4:38)] 
row.names(player_biplot) <- player_biplot[,1]
player_biplot <- player_biplot [, c(2:36)] 

player_biplot <- knnImputation(data = player_biplot, k = 5, scale = T, meth="weighAvg")

all <- player_biplot[,c(2:35)]
goalkeeper <- player_biplot [, c(31,32,33,34,35)]
defensor <- player_biplot [, c(19,21,24,28,29,30)]
middle <- player_biplot [, c(3,5,6,8,9,11,12,18,20,22,28)]
scorer <- player_biplot [, c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,20,22,25,26,27)]

#all
heatmap(abs(cor(all)), Rowv=NA, Colv=NA, revC=TRUE)
corrplot(cor(all))

#porter
heatmap(abs(cor(goalkeeper)), Rowv=NA, Colv=NA, revC=TRUE)
corrplot(cor(goalkeeper))

#defensa

heatmap(abs(cor(defensor)), Rowv=NA, Colv=NA, revC=TRUE)
corrplot(cor(defensor))

#migcamp
heatmap(abs(cor(middle)), Rowv=NA, Colv=NA, revC=TRUE)
corrplot(cor(middle))

#davanter
heatmap(abs(cor(scorer)), Rowv=NA, Colv=NA, revC=TRUE)
corrplot(cor(scorer))

library(ggfortify)

ggplot2::autoplot(stats::prcomp(all, scale=TRUE, center = T), label = FALSE, loadings = TRUE, loadings.label.colour='brown', loadings.label = TRUE, loadings.label.size = 4,
                  loadings.label.repel=TRUE) 



#densities 

# source: https://www.kaggle.com/peterh/density-plot-craziness-futbol-leagues?scriptVersionId=308586
#nothing special tbh, #i don't really think they are relevant at all


source("loadingDB.R")
match.trim <- match[,c(1,2,3,4,5,6,56,57,58,59,60,61,62,63,64,65,66)]
matchplayerid<- match.trim[rowSums(is.na(match.trim))!=11,]

p1 <- matchplayerid[,c(3,7)]
p2 <- matchplayerid[,c(3,8)]
p3 <- matchplayerid[,c(3,9)]
p4 <- matchplayerid[,c(3,10)]
p5 <- matchplayerid[,c(3,11)]
p6 <- matchplayerid[,c(3,12)]
p7 <- matchplayerid[,c(3,13)]
p8 <- matchplayerid[,c(3,14)]
p9 <- matchplayerid[,c(3,15)]
p10 <- matchplayerid[,c(3,16)]
p11 <- matchplayerid[,c(3,17)]


p1$playerid <- p1$home_player_1[,-2]
p2$playerid <- p2$home_player_2[,-2]
p3$playerid <- p3$home_player_3[,-2]
p4$playerid <- p4$home_player_4[,-2]
p5$playerid <- p5$home_player_5[,-2]
p6$playerid <- p6$home_player_6[,-2]
p7$playerid <- p7$home_player_7[,-2]
p8$playerid <- p8$home_player_8[,-2]
p9$playerid <- p9$home_player_9[,-2]
p10$playerid <- p10$home_player_10[,-2]
p11$playerid <- p11$home_player_11[,-2]

players_long <- bind_rows(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11)
pl_narm<- players_long[is.na(players_long$playerid)==FALSE,]
plid <- distinct(pl_narm)
plid$player_api_id <- plid$playerid
plid <- plid[,-2]
player.league <- left_join(players, plid)
league.grouped <- player.league %>% group_by(league_id) %>% arrange(league_id)
player.league$league_id <- as.factor(player.league$league_id)
player.league$birthday <- as.Date(player.league$birthday)

#overkill
levels(player.league$league_id)[levels(player.league$league_id)== 1] <- "Belgium Jupiler League"
levels(player.league$league_id)[levels(player.league$league_id)== 1729] <- "England Premier League"
levels(player.league$league_id)[levels(player.league$league_id)== 4769] <- "France Ligue 1"
levels(player.league$league_id)[levels(player.league$league_id)== 7809] <- "Germany 1. Bundesliga1"
levels(player.league$league_id)[levels(player.league$league_id)== 10257] <- "Italy Serie A"
levels(player.league$league_id)[levels(player.league$league_id)== 13274] <- "Netherlands Eredivisie"
levels(player.league$league_id)[levels(player.league$league_id)== 15722] <- "Poland Ekstraklasa"
levels(player.league$league_id)[levels(player.league$league_id)== 17642] <- "Portugal Liga ZON Sagres"
levels(player.league$league_id)[levels(player.league$league_id)== 19694] <- "Scotland Premier League"
levels(player.league$league_id)[levels(player.league$league_id)== 21518] <- "Spain LIGA BBVA"
levels(player.league$league_id)[levels(player.league$league_id)== 24558] <- "Switzerland Super League"

fileName <- paste0("factorsImages/playerStats/","weight", ".png")
png(filename = fileName, bg="transparent")
ggPlot <- ggplot(player.league, aes(weight)) + 
  geom_density(aes(color=league_id)) + 
  scale_color_brewer(palette="Paired")
print(ggPlot)
dev.off()

fileName <- paste0("factorsImages/playerStats/","height", ".png")
png(filename = fileName, bg="transparent")
ggPlot <- ggplot(player.league, aes(height)) + 
  geom_density(aes(color=league_id)) + 
  scale_color_brewer(palette="Paired")
print(ggPlot)
dev.off()

fileName <- paste0("factorsImages/playerStats/","birthday", ".png")
png(filename = fileName, bg="transparent")
ggPlot <- ggplot(player.league, aes(birthday)) + 
  geom_density(aes(color=league_id)) + 
  scale_color_brewer(palette="Paired")
print(ggPlot)
dev.off()

player.stats <- left_join(player.league, player_attrs, by= "player_api_id")

player.stats <- player.stats[,c(2,3,5,6,7,8,11:49)]

player.stats.names <- player.stats[8:45]
for(i in player.stats.names){
  fileName <- paste0("factorsImages/playerStats/",i, ".png")
  png(filename = fileName, bg="transparent")
  ggPlot <- ggplot(player.stats, aes(i)) + 
    geom_density(aes(color=league_id)) +
    scale_color_brewer(palette="Paired") 
  print(ggPlot)
  dev.off()
}


