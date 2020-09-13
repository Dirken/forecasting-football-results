#######################
#Card
#######################
cards  <- incidents %>% filter(type == 'card')
cards %>%
  ggplot(mapping = aes(x = half_elapsed)) +
  geom_bar() +
  facet_wrap(~half, scales = "free_x")