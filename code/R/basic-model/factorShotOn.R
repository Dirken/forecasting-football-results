#######################
#ShotOn
#######################
shoton  <- incidents %>% filter(type == 'shoton')
shoton
table(shoton$subtype1)
table(shoton$subtype2)
shoton$type <- NULL

data <- unique(shoton$subtype1)
for(i in data){
  fileName <- paste0("factorsImages/shotOn/",i, ".png")
  png(filename = fileName, bg="transparent")
  ggPlot <- pitch + shoton %>% 
    filter(subtype1 == i) %>%
    geom_jitter(mapping = aes(colour = subtype1), alpha = 0.5, size = 4, stroke = 0) +
    #scale_color_viridis(discrete = T) +
    guides(colour = guide_legend(override.aes = list(alpha = 1))) +
    theme(legend.position = "bottom")
  print(ggPlot)
  dev.off()
}


View(shoton)


pitch + shoton %>% 
  geom_jitter(mapping = aes(colour = subtype1), alpha = 0.3, size = 2, stroke = 0) +
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  theme(legend.position = "bottom")

shoton %>%
  ggplot(mapping = aes(x = lon)) +
  geom_bar(mapping = aes(fill = subtype1)) +
  #scale_color_viridis(discrete = T) +
  ggtitle("Shots On by longitude")


IfIfIfshoton %>%
  ggplot(mapping = aes(x = lat)) +
  geom_bar(mapping = aes(fill = subtype1)) +
  #scale_color_viridis(discrete = T)+
  ggtitle("Shots On by latitude")
