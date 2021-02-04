
#######################
#ShotOff
#######################
shotoff  <- incidents %>% filter(type == 'shotoff')

table(shotoff$subtype1)
table(shotoff$subtype2)
shotoff$type <- NULL

data <- unique(shotoff$subtype1)
for(i in data){
  fileName <- paste0("./factorsImages/shotOff/",i, ".png")
  png(filename = fileName, bg="transparent", width = 1200, height = 860)
  ggPlot <- pitch + shotoff %>% 
    filter(subtype1 == i) %>%
    geom_jitter(mapping = aes(colour = subtype1), alpha = 0.4, size = 2, stroke = 0) +
    #scale_color_viridis(discrete = T) +
    guides(colour = guide_legend(override.aes = list(alpha = 1))) +
    theme(legend.position = "bottom")
  print(ggPlot)
  dev.off()
}

pitch + shotoff %>% 
  geom_jitter(mapping = aes(colour = subtype1), alpha = 0.3, size = 2, stroke = 0) +
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  theme(legend.position = "bottom")


shotoff %>%
  ggplot(mapping = aes(x = lon)) +
  geom_bar(mapping = aes(fill = subtype1)) +
  #scale_color_viridis(discrete = T) +
  ggtitle("Shots Off by longitude")

shotoff %>%
  ggplot(mapping = aes(x = lat)) +
  geom_bar(mapping = aes(fill = subtype1)) +
  #scale_color_viridis(discrete = T)+
  ggtitle("Shots Off by latitude")
