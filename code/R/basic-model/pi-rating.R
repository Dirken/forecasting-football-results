library(piratings)
install.packages("piratings")
data("EPL2008_2015")

## prepare the function arguments:
teams <- as.matrix(EPL2008_2015[, c("home_team", "away_team")])
outcomes <- as.matrix(EPL2008_2015[, c("home_goals", "away_goals")])

grid <- optimize_pi_ratings(teams, outcomes, seq(0.04, 0.08, 0.005), seq(0.3, 0.7, 0.05))


## we plot this grid using the ggplot2 library
library(ggplot2)

ggplot(data = grid, aes(x = lambda, y = gamma, fill = mean.squared.error)) + 
  geom_tile() + scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 2.668) + 
  labs(x = "lambda", y = "gamma", title = "grid optimization", fill = "Mean \nsquared \nerror") + 
  theme(plot.title = element_text(hjust = 0.5))

piratings <- calculate_pi_ratings(teams, outcomes, 0.06, 0.6)

piratings
