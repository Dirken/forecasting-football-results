
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
  png(filename = fileName, bg="transparent")
  ggPlot <- shotoff %>% 
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
