con =  dbConnect(SQLite(), dbname="../../../datasets/european-football-db/database.sqlite")
matches        <- tbl_df(dbGetQuery(con,"SELECT season,home_team_goal,away_team_goal FROM Match"))

table_match_goals= matches %>% mutate(total_goals=home_team_goal+away_team_goal) %>% group_by(total_goals) %>% summarise(n=n())

goals_df=as.data.frame(table_match_goals)


number_of_matches=matches %>% count()
number_of_matches=as.data.frame(number_of_matches)
number_of_matches=as.integer(number_of_matches)
print(number_of_matches)


av_number_goals=goals_df %>% mutate(number_goals=total_goals*n)


av_goals=sum(av_number_goals$number_goals)/number_of_matches
print(av_goals)


lambda=av_goals

# Plot probability
goals_df=goals_df %>% mutate(p=n/number_of_matches) %>%mutate(pois=dpois(total_goals, lambda))
print(goals_df)

p1=ggplot(goals_df, aes(total_goals))+ geom_point(aes(y=p,color="Goals"))+geom_point(aes(y=pois,color="Poisson"))
p2=p1+geom_line(aes(y=p,color="Goals"))+ geom_line(aes(y=pois,color="Poisson")) + ggtitle("Poisson distribution vs Goals")
p2 + ylab("Goal Probability")



