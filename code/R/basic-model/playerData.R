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

