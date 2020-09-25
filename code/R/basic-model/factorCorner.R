#######################
#Corner
#######################
corner  <- incidents %>% filter(type == 'corner')

coords <- unique(corner[c("lon", "lat")])
min(!is.na(corner$lat))
max(!is.na(corner$lon))
table(corner$lon)
table(corner$lat)
corner %>%
  ggplot(mapping = aes(x = half_elapsed)) +
  geom_bar(mapping = aes(fill = subtype1)) +
  scale_color_viridis(discrete = T) +
  facet_wrap(~half, scales = "free_x") +
  ggtitle("hjopa")

 
pitch + corner %>%
  geom_jitter(mapping = aes(colour = subtype1), alpha = 0.3, size = 2, stroke = 0) +
  scale_color_viridis(discrete = T)


#are crosses associated with a bigger win %?

View(corner)













