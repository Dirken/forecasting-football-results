#######################
#throwin (pase de banda)
#######################
throwin <- incidents %>% filter(type == 'throwin')

names(throwin)
table(throwin$subtype1) #all are crosses....
table(throwin$subtype2)
throwin$subtype2 <- NULL
unique(throwin$lon)
unique(throwin$lat)
unique(incidents$type)

pitch + throwin %>%
  geom_jitter(mapping = aes(colour = subtype1), alpha = 0.3, size = 2, stroke = 0) +
  guides(colour = guide_legend(override.aes = list(alpha = 1))) 

#discarded, same as crosses!




throwin
