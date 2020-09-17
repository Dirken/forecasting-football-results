#######################
#ShotOn
#######################
shoton  <- incidents %>% filter(type == 'shoton')

table(shoton$subtype1)
table(shoton$subtype2)
shoton$type <- NULL

data <- unique(shoton$subtype1)
for(i in data){
  fileName <- paste0("factorsImages/fouls/",i, ".png")
  png(filename = fileName, bg="transparent")
  ggPlot <- shoton %>% 
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


head(shoton,10)
