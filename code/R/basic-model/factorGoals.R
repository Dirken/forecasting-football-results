#######################
#Goal
#######################
goals  <- incidents %>% filter(type == 'goal')
goals %>%
  ggplot(mapping = aes(x = half_elapsed)) +
  geom_bar(mapping = aes(fill = subtype1)) +
  scale_color_viridis(discrete = T) +
  facet_wrap(~half, scales = "free_x")

goals %>%
  ggplot(mapping = aes(x = lat)) +
  geom_bar(mapping = aes(fill = subtype1)) +
  scale_color_viridis(discrete = T)

goals %>%
  ggplot(mapping = aes(x = lon)) +
  geom_bar(mapping = aes(fill = subtype1)) +
  scale_color_viridis(discrete = T)

goals %>%
  ggplot(mapping = aes(x = lon, y = lat)) +
  geom_jitter(mapping = aes(colour = subtype1), alpha = 0.3, size = 2, stroke = 0) +
  scale_color_viridis(discrete = T) +
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  facet_wrap(~half) +
  theme_minimal()

narrowmatch  <- match %>%
  select(id, country_id, league_id, season, home_team_api_id, away_team_api_id, home_team_goal, away_team_goal)

goals  <- incidents %>% 
  filter(type == 'goal') %>% 
  inner_join(narrowmatch, by = c('game_id' = 'id'), copy = T)

goals %<>% mutate(scored_by = ifelse(team == home_team_api_id, 'home', 'away'))
goals %<>%
  mutate(lat = ifelse(half == 1, lat, 70 - lat),
         lon = ifelse(half == 1, lon, 46 - lon))

pitch.l = 105
pitch.w = 68

add_rectangle <- function(orig, miny, maxy, width) { 
  mid <- pitch.w / 2 
  halfwidth <- width / 2 
  rbind(orig,
        c(x1 = mid - halfwidth, y1 = miny, x2 = mid + halfwidth, y2 = miny),
        c(x1 = mid - halfwidth, y1 = miny, x2 = mid - halfwidth, y2 = maxy),
        c(x1 = mid - halfwidth, y1 = maxy, x2 = mid + halfwidth, y2 = maxy),
        c(x1 = mid + halfwidth, y1 = maxy, x2 = mid + halfwidth, y2 = miny)
  )
}

field <- tibble(x1 = 0, y1 = pitch.l / 2, x2 = pitch.w, y2 = pitch.l / 2) %>%
  add_rectangle(0, pitch.l, pitch.w) %>%
  add_rectangle(0, -2.44, 7.32) %>%
  add_rectangle(pitch.l, pitch.l + 2.44, 7.32) %>%
  add_rectangle(0, 5.5, 18.32) %>%
  add_rectangle(pitch.l, pitch.l - 5.5, 18.32) %>%
  add_rectangle(0, 16.5, 40.32) %>%
  add_rectangle(pitch.l, pitch.l - 16.5, 40.32) * (2/3)


penalty_curve_bottom  <- tibble(x1 = (pitch.w / 2) - 7.312, y1 = 16.5,
                                x2 = (pitch.w / 2) + 7.312, y2 = 16.5) * (2/3)
penalty_curve_top  <- tibble(x1 = (pitch.w / 2) - 7.312, y1 = pitch.l - 16.5,
                             x2 = (pitch.w / 2) + 7.312, y2 = pitch.l - 16.5) * (2/3)
corner_bottom <- tibble(x1 = c(0, pitch.w - 1), y1 = c(1, 0),
                        x2 = c(1, pitch.w)    , y2 = c(0, 1)) * (2/3)
corner_top <- tibble(x1 = c(0, pitch.w - 1), y1 = c(pitch.l -1, pitch.l    ),
                     x2 = c(1, pitch.w    ), y2 = c(pitch.l   , pitch.l - 1)) * (2/3)
circle  <- tibble(x1 = (pitch.w / 2) - 9.15, y1 = pitch.l / 2,
                  x2 = (pitch.w / 2) + 9.15, y2 = pitch.l / 2) * (2/3)

pitch.l = pitch.l * (2/3)
pitch.w = pitch.w * (2/3)
pitch.aes  <- aes(x = x1, y = y1, xend = x2, yend = y2)

pitch  <- ggplot(mapping = aes(x = lon, y = lat)) +
  field                   %>% geom_segment(mapping = pitch.aes, colour = 'white') +
  circle                  %>% geom_curve(  mapping = pitch.aes, colour = 'white', curvature = 1, ncp = 20) +
  circle                  %>% geom_curve(  mapping = pitch.aes, colour = 'white', curvature = -1, ncp = 20) +
  penalty_curve_bottom    %>% geom_curve(  mapping = pitch.aes, colour = 'white', curvature = -0.4) +
  penalty_curve_top       %>% geom_curve(  mapping = pitch.aes, colour = 'white', curvature = 0.4) +
  corner_bottom           %>% geom_curve(  mapping = pitch.aes, colour = 'white', curvature = -0.5) +
  corner_top              %>% geom_curve(  mapping = pitch.aes, colour = 'white', curvature = 0.5) +
  coord_fixed((pitch.l/pitch.w)/1.546) +
  theme_dark() +
  scale_x_continuous(breaks = 1:10 / 10 * pitch.w, labels = NULL) +
  scale_y_continuous(breaks = 1:22 / 22 * pitch.l, labels = NULL) +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank())
pitch +
  goals %>% geom_jitter(mapping = aes(colour = scored_by), alpha = 0.3, size = 2, stroke = 0) +
  scale_color_viridis(discrete = T) +
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  labs(title = "Scoring positions") +
  facet_wrap(~half, labeller = label_both)
