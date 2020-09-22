#########################################################
#Team data and attributes
#########################################################
team_attrs <- tbl_df(dbGetQuery(con, "SELECT * FROM Team_Attributes"))

names(team_attrs) 
team_attrs$id <- NULL
team_attrs$buildUpPlayDribbling <- NULL


#first what we can see is that we have plenty of categorical values.
#This values come from a classification on the variables we have the numerical values for instance:
team_attrs$buildUpPlaySpeed
team_attrs$buildUpPlaySpeedClass 
#this is the category that fifa does and as we can see, are not really significative
table(team_attrs$buildUpPlaySpeedClass) #only 3 categories, most of them on one...
#This happen across all variables, therefore, we are going to drp them.

team_attrs <- team_attrs %>%
  select(-ends_with("Class")) # Reduced by half our variables :) 13 left. What this vars do?

View(team_attrs)# this table is pretty interesting because I didn't expect to have data from different years.
#we can see that in this dataset, if a team is progressing and we could even calculate an average per year or so..

#lets visualize an example:
library(lubridate)
team_attrs$date <- year(ymd_hms(team_attrs$date))
team_attrs$
team_attrs$team_api_id

team_attrsPlot <- head(team_attrs,58)
ggplot(data = team_attrsPlot, aes(x = date, y = buildUpPlaySpeed, color=factor(team_api_id), group = team_api_id)) +
  geom_point() +
  geom_line() +
  theme(legend.position = "bottom")+
  labs(x = "Years",
       y = "Build Up Play Speed",
       title = "How improves team data across the years")



#can we get an overall rating of a team?
library("corrplot")
corrPlot <- corrplot(cor(team_attrs[4:length(team_attrs)]))

team_attrsAVG <- team_attrs %>%
  select(-c("team_fifa_api_id", 
             "team_api_id", 
             "date")) %>%
  mutate(Mean = rowMeans(.))
