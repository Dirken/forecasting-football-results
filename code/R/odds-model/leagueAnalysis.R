##############################################
#How does win ratio hold across local/visitor?
##############################################
#we convert results between win/draw/lose according to the local.
match$result <- "NULL"
match$result[match$home_team_goal == match$away_team_goal] <- "Draw"
match$result[match$home_team_goal > match$away_team_goal] <- "Local Win"
match$result[match$home_team_goal < match$away_team_goal] <- "Visitor Win"

data <- as.data.frame(table(match$result))

bp<- ggplot(data, aes(x="", y=Freq, fill=Var1)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  labs(fill = "Category",
       x = NULL,
       y = NULL,
       title = "Win % From all the leagues") + 
  geom_text(aes(label = paste(round(Freq / sum(Freq) * 100, 1), "%")), position = position_stack(vjust = 0.5)) +
  theme(legend.position="bottom")
bp

fileName <- paste0("images/missingValues/pie",".jpeg")
jpeg(file=fileName)
bp
dev.off()

##############################################  
#Does this hold for all leagues??
##############################################
library(tidyverse)
dataLeagues <- match %>% 
  select(result, league_id) %>% 
  group_by(league_id) %>%   
  mutate(drawPer = 100*sum(result == "Draw") / n(),
            winPer = 100*sum(result == "Local Win") / n(),
            losePer = 100*sum(result == "Visitor Win") / n()
  ) %>%
  pivot_longer(cols = c(drawPer, winPer,losePer), names_to = "Category",  values_to = "Freq")

bp<- ggplot(dataLeagues, aes(x="", y=Freq, fill=result)) +
  geom_bar(width = 1, stat = "identity", position = 'fill') +
  coord_polar("y", start=0) +
  facet_wrap(~ league_id)+  
  labs(fill = "Category",
       x = NULL,
       y = NULL,
       title = "Win % From all the leagues") +
  theme(legend.position="bottom")
 # work on this later to have the labels but not so important.
 # %>%geom_text(aes(label = Freq, position = position_stack(vjust = 0.5)))

fileName <- paste0("images/missingValues/multiPie",".jpeg")
jpeg(file=fileName, width = 2000, height = 2000)
bp #takes a while to do :(
dev.off()

##############################################
#Is there a balance in data across countries?
##############################################
matchData <- dbGetQuery(con,"SELECT Country.name AS country_name,  League.name, count(*) as total
                        FROM Match
                        JOIN Country on Country.id = Match.country_id
                        JOIN League on League.id = Match.league_id
                        LEFT JOIN Team AS HT on HT.team_api_id = Match.home_team_api_id
                        LEFT JOIN Team AS AT on AT.team_api_id = Match.away_team_api_id
                        GROUP BY Country.name, League.name")

##############################################
#Number of Matches in each league by season: 
#we can see some inbalances between leagues
##############################################
ggplot(data=matchData, aes(x=country_name, y=total, group=name, label = total, fill=name)) +
  geom_bar(stat="identity") +
  ggtitle("Number of total matches per each league in total") +
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  theme(legend.position = "bottom")
  
##############################################
#How average goals are across leagues?
##############################################
leagueSummary <- dbGetQuery(con,
                            "SELECT Country.name AS country_name, 
                            League.name AS league_name, 
                            season,
                            count(distinct stage) AS number_of_stages,
                            count(distinct HT.team_long_name) AS number_of_teams,
                            avg(home_team_goal) AS avg_home_team_scors, 
                            avg(away_team_goal) AS avg_away_team_goals, 
                            avg(home_team_goal-away_team_goal) AS avg_goal_dif, 
                            avg(home_team_goal+away_team_goal) AS avg_goals, 
                            sum(home_team_goal+away_team_goal) AS total_goals                                       
                            FROM Match
                            JOIN Country on Country.id = Match.country_id
                            JOIN League on League.id = Match.league_id
                            LEFT JOIN Team AS HT on HT.team_api_id = Match.home_team_api_id
                            LEFT JOIN Team AS AT on AT.team_api_id = Match.away_team_api_id
                            GROUP BY Country.name, League.name, season
                            HAVING count(distinct stage) > 10
                            ORDER BY Country.name, League.name, season DESC
                            ;")
##############################################
#Avg_Goals per season in all leagues:
##############################################
leagueSummary <- dbGetQuery(con,
                            "SELECT Country.name AS country_name, 
                            League.name AS league_name, 
                            season,
                            count(distinct stage) AS number_of_stages,
                            count(distinct HT.team_long_name) AS number_of_teams,
                            avg(home_team_goal) AS avg_home_team_scors, 
                            avg(away_team_goal) AS avg_away_team_goals, 
                            avg(home_team_goal-away_team_goal) AS avg_goal_dif, 
                            avg(home_team_goal+away_team_goal) AS avg_goals, 
                            sum(home_team_goal+away_team_goal) AS total_goals                                       
                            FROM Match
                            JOIN Country on Country.id = Match.country_id
                            JOIN League on League.id = Match.league_id
                            LEFT JOIN Team AS HT on HT.team_api_id = Match.home_team_api_id
                            LEFT JOIN Team AS AT on AT.team_api_id = Match.away_team_api_id
                            GROUP BY Country.name, League.name, season
                            HAVING count(distinct stage) > 10
                            ORDER BY Country.name, League.name, season DESC
                            ;")

leagueSummaryTopLeagues <- dbGetQuery(con,
                            "SELECT Country.name AS country_name, 
                            League.name AS league_name, 
                            season,
                            count(distinct stage) AS number_of_stages,
                            count(distinct HT.team_long_name) AS number_of_teams,
                            avg(home_team_goal) AS avg_home_team_scors, 
                            avg(away_team_goal) AS avg_away_team_goals, 
                            avg(home_team_goal-away_team_goal) AS avg_goal_dif, 
                            avg(home_team_goal+away_team_goal) AS avg_goals, 
                            sum(home_team_goal+away_team_goal) AS total_goals                                       
                            FROM Match
                            JOIN Country on Country.id = Match.country_id
                            JOIN League on League.id = Match.league_id
                            LEFT JOIN Team AS HT on HT.team_api_id = Match.home_team_api_id
                            LEFT JOIN Team AS AT on AT.team_api_id = Match.away_team_api_id
                            WHERE country_name in ('Spain', 'Germany', 'France', 'Italy', 'England')
                            GROUP BY Country.name, League.name, season
                            HAVING count(distinct stage) > 10
                            ORDER BY Country.name, League.name, season DESC
                            ;")

leagueSummaryMinorLeagues <- dbGetQuery(con,
                                      "SELECT Country.name AS country_name, 
                            League.name AS league_name, 
                            season,
                            count(distinct stage) AS number_of_stages,
                            count(distinct HT.team_long_name) AS number_of_teams,
                            avg(home_team_goal) AS avg_home_team_scors, 
                            avg(away_team_goal) AS avg_away_team_goals, 
                            avg(home_team_goal-away_team_goal) AS avg_goal_dif, 
                            avg(home_team_goal+away_team_goal) AS avg_goals, 
                            sum(home_team_goal+away_team_goal) AS total_goals                                       
                            FROM Match
                            JOIN Country on Country.id = Match.country_id
                            JOIN League on League.id = Match.league_id
                            LEFT JOIN Team AS HT on HT.team_api_id = Match.home_team_api_id
                            LEFT JOIN Team AS AT on AT.team_api_id = Match.away_team_api_id
                            WHERE country_name not in ('Spain', 'Germany', 'France', 'Italy', 'England')
                            GROUP BY Country.name, League.name, season
                            HAVING count(distinct stage) > 10
                            ORDER BY Country.name, League.name, season DESC
                            ;")



#average goals per game over time on all leagues
ggplot(leagueSummary, aes(x=season, y=avg_goals, color=league_name, group = league_name))+
  geom_point() +
  geom_line() +
  theme(legend.position = "bottom")

#average goals per game over time on major leagues
ggplot(leagueSummaryTopLeagues, aes(x=season, y=avg_goals, color=league_name, group = league_name))+
  geom_point() +
  geom_line() + 
  theme(legend.position = "bottom")


#average goals per game over time on minor leagues
ggplot(leagueSummaryMinorLeagues, aes(x=season, y=avg_goals, color=league_name, group = league_name))+
  geom_point() +
  geom_line() +
  theme(legend.position = "bottom")


##############################################
#Avg_Goals per season in all leagues:
#diff between home - out
##############################################

#Average goals difference between home - out, we can see that in any case home is bigger
ggplot(leagueSummary, aes(x=season, y=avg_home_team_scors - avg_away_team_goals, color=league_name, group = league_name))+
  geom_point() +
  geom_line() +
  theme(legend.position = "bottom")

ggplot(leagueSummaryTopLeagues, aes(x=season, y=avg_home_team_scors - avg_away_team_goals, color=league_name, group = league_name))+
  geom_point() +
  geom_line() + ggtitle("Difference of goals scored and received by local team") +
  theme(legend.position = "bottom")

ggplot(leagueSummaryMinorLeagues, aes(x=season, y=avg_home_team_scors - avg_away_team_goals, color=league_name, group = league_name))+
  geom_point() +
  geom_line() + ggtitle("Difference of goals scored and received by local team") +
  theme(legend.position = "bottom")


##############################################
#Correlation between home/away
##############################################
#is there a correlation between home form and away form? a good team is always a good team?
team   <- select(team, team_api_id, team_long_name, team_short_name)
match  <- select(match, league_id, home_team_api_id, away_team_api_id, home_team_goal, away_team_goal)

matchPoints <- match %>% 
  mutate(home_team_points = if_else((home_team_goal > away_team_goal),3,if_else((home_team_goal == away_team_goal),1,0))) %>%
  mutate(away_team_points = if_else((home_team_goal > away_team_goal),0,if_else((home_team_goal == away_team_goal),1,3))) 

homePoints <- matchPoints %>%
  select(league_id, team_api_id = home_team_api_id, home_team_points) %>%
  group_by(league_id, team_api_id) %>%
  summarize(avgHome = mean(home_team_points))

awayPoints <- matchPoints %>%
  select(league_id, team_api_id = away_team_api_id, away_team_points) %>%
  group_by(league_id, team_api_id) %>%
  summarize(avgAway = mean(away_team_points))

points <- left_join(homePoints, awayPoints, by = c("league_id", "team_api_id"))

points <- points %>%
  mutate(avgPointsGame = (avgHome + avgAway)/2)

points <- left_join(points, league, by = "league_id")
points <- left_join(points, team, by = "team_api_id")


home_away <- points %>%
  mutate(ratioHA = avgHome / avgAway) %>%
  select(team_long_name, league_id, league_name, avgPointsGame, ratioHA) %>%
  as.data.frame()

ggplot(home_away, aes(x = ratioHA, y = avgPointsGame)) + 
  geom_point() +
  ylim(0,3) +
  geom_point(aes(x = 1, y = 3), color="red") +
  geom_text(aes(x = 1, y = 3), hjust = 0, nudge_x = 0.05, size = 3, label = "Perfection") +
  labs(title = "Comparison of Average Points Per Game (PPG) for each Team\nin Home and Away Games",
       x = "Home to Away Points Ratio",
       y = "Average PPG") +
  theme(plot.title = element_text(hjust = 0.5))


ggplot(home_away, aes(x = league_name, y = ratioHA)) + 
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Comparison of Home to Away Points Ratio for each League",
       x = "League Name",
       y = "Home to Away Points Ratio") +
  theme(plot.title = element_text(hjust = 0.5))

#with the name of the team.
ggplot(home_away, aes(x = league_name, y = ratioHA)) + 
  geom_point() +
  geom_text(aes(label=team_long_name),hjust=0, vjust=0) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Comparison of Home to Away Points Ratio for each League",
       x = "League Name",
       y = "Home to Away Points Ratio") +
  theme(plot.title = element_text(hjust = 0.5))
#we can see that the teams that are in the top are teams that are outliers but that still there is a trend
#what might be also interesting is to actually see how many points do teams punctuate


##############################################
#Points per season per team:
#Are leagues very equal? Is inequality a thing?
##############################################

league <- tbl_df(dbGetQuery(con,"SELECT * FROM League"))
team   <- tbl_df(dbGetQuery(con,"SELECT * FROM Team"))
match  <- tbl_df(dbGetQuery(con,"SELECT * FROM Match"))

league <- select(league, id, name, country_id) %>% rename(league_id = id, league_name = name)
team   <- select(team, team_api_id, team_long_name, team_short_name)
match  <- select(match, league_id, home_team_api_id, away_team_api_id, home_team_goal, away_team_goal, season)

points <- match %>% 
  mutate(home_team_points = if_else((home_team_goal > away_team_goal),3,if_else((home_team_goal == away_team_goal),1,0))) %>%
  mutate(away_team_points = if_else((home_team_goal > away_team_goal),0,if_else((home_team_goal == away_team_goal),1,3))) 

points <- left_join(points, league, by = "league_id")


#here solve the name thing that should appear
lp <- points %>% 
  select(league_name, league_id, id = home_team_api_id, home_team_points, season) %>%
  group_by(league_id, id, season)%>%
  summarize(points = sum(home_team_points))

lp2 <- points %>% 
  select(league_name, league_id, id = away_team_api_id, away_team_points, season) %>%
  group_by(league_id, id, season)%>%
  summarize(points = sum(away_team_points))


all <- merge(lp,lp2, by=c("league_id", "season", "id"))
all$totalPoints <- all$points.x + all$points.y

for(i in unique(all$league_id)){
  print(i)
  difference = max(all$totalPoints[all$league_id == i]) - min(all$totalPoints[all$league_id == i])
  print(difference)
}

all$points.x
#podriem posar el enlloc del id el nom i tal però bueno.




