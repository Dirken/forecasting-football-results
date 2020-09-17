#######################
#Cross
#######################
cross  <- incidents %>% filter(type == 'cross')

names(cross)
table(cross$subtype1)
table(cross$subtype2)
cross$subtype1 <- NULL #any subtype
cross$subtype2 <- NULL
cross$type <- NULL #all is cross obviously

table(cross$elapsed)
table(cross$elapsed_plus)
table(cross$half_elapsed)


cross %>%
  ggplot(mapping = aes(x = lon, y = lat)) +
  geom_jitter(mapping = aes(colour = "cross"), alpha = 0.3, size = 2, stroke = 0) +
  scale_color_viridis(discrete = T) +
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  facet_wrap(~half) +
  theme_minimal()

