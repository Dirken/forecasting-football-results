Creating an expected goals model

library(StatsBombR)
library(dplyr)
library(data.table)
library(scales)
library(caret)
library(RANN)
library(plotROC)
library(tidyr)
library(reshape2)
library(MLmetrics)
library(lubridate)
library(gridExtra)
library(cowplot)
library(ggthemes)
source('pitch.R')

#Reading in the events (37 and 49 are the women's competition)

Comp = FreeCompetitions()

#Getting data only for the women's competitions
Matches = FreeMatches(c(37, 49))


all_events = data.frame()
for(i in 1:nrow(Matches)){
  if(i %in% c(94)) next
  temp = get.matchFree(Matches[i,])
  print(i)
  temp = allclean(temp)
  all_events = bind_rows(all_events, temp)
  rm(temp)
}

set.seed(42)

#Flagging all possessions with a through ball and dribbles
all_events = all_events %>% group_by(match_id, possession) %>%
  mutate(tb.flag = any(pass.through_ball == T),
         shot.flag = any(type.name == 'Shot'),
         dribble.flag = any(dribble.outcome.name == 'Complete'),
         tb.time = ifelse(shot.flag == T & pass.through_ball == T, TimeInPoss, NA),
         shot.time = ifelse(type.name == 'Shot', TimeInPoss, NA),
         dribble.time = ifelse(shot.flag == T & dribble.outcome.name == 'Complete', TimeInPoss, NA)) %>%
  fill(tb.time, shot.time, dribble.time) %>%
  mutate(dribble.to.shot.time = shot.time - dribble.time,
         tb.to.shot.time = shot.time - tb.time)

#filtering for shots, for some EDA
shots = filter(all_events, type.name == 'Shot')
shots$is.goal = ifelse(shots$shot.outcome.name == 'Goal', "1","0")


#Plotting them 
p = create_StatsBomb_Pitch("#ffffff", "#A9A9A9", "#ffffff", "#000000", BasicFeatures=FALSE)

p + geom_point(data = shots, aes(x = location.x, y = location.y), alpha = 0.2, size = 2,  shape = 16) + theme_void()

#cleaning the data to remove any column with only NAs
not_all_na = function(x) any(!is.na(x))
shots$is.goal = ifelse(shots$shot.outcome.name == 'Goal', "Goal","NoGoal")
shots_valid = shots %>% select_if(not_all_na)

#Creating location variables
shots_valid = shots_valid %>% mutate(opposite = 120 - location.x,
                                     adjacent = 40 - location.y,
                                     hypotenuse = sqrt(opposite^2 + adjacent^2),
                                     angle.to.goal = ifelse(location.y > 40, 180 - asin(opposite/hypotenuse)*180/3.14, asin(opposite/hypotenuse)*180/3.14),
                                     distance.to.goal = hypotenuse) %>%
  select(-c(opposite, adjacent, hypotenuse))

#adding distance from goalkeeper and angle to gk
shots_valid = shots_valid %>% mutate(opposite = location.x.GK - location.x,
                                     adjacent = location.y.GK - location.y,
                                     hypotenuse = sqrt(opposite^2 + adjacent^2),
                                     angle.to.gk = ifelse(location.y > location.y.GK, 180 - asin(opposite/hypotenuse)*180/3.14, asin(opposite/hypotenuse)*180/3.14),
                                     angle.to.gk = ifelse(location.x > location.x.GK & location.y < location.y.GK, 270 - asin(opposite/hypotenuse)*180/3.14, angle.to.gk),
                                     distance.to.gk = hypotenuse,
                                     gk.to.goalline = sqrt((120 - location.x.GK)^2 + (40 - location.y.GK)^2)) %>%
  select(-c(opposite, adjacent, hypotenuse))

#There are lots of columns with TRUE and NA. First replacing these NA with FALSE
logical.vars = names(Filter(is.logical, shots_valid))
df = shots_valid[logical.vars]
df[is.na(df)] = FALSE
shots_valid[logical.vars] = df

#choosing independent variables
ind.vars = c('id', 'is.goal','distance.to.gk', 'distance.to.goal', 'angle.to.gk', 'angle.to.goal', 'gk.to.goalline', 'play_pattern.name','shot.body_part.name', 'shot.type.name', 'shot.technique.name')

shots.varsdata = subset(shots_valid, select = ind.vars) %>% drop_na()

#splitting into test train
idx = createDataPartition(shots.varsdata$is.goal, p = 0.8, list = F)
train = shots.varsdata[idx,]
test = shots.varsdata[-idx,]

training_xg = shots.varsdata$shot.statsbomb_xg[idx]
test_xg = subset(shots_valid, select = c(ind.vars, 'shot.statsbomb_xg')) %>% drop_na() %>% select(-ind.vars) %>% slice(-idx)

library(doParallel)
cl = makePSOCKcluster(5)
registerDoParallel(cl)

vars = ncol(model.matrix(is.goal ~ ., train[,!colnames(train) %in% c("id")])) - 2
grid = expand.grid(mtry = 4:vars)

control = trainControl(classProbs = TRUE, method = "cv", number = 5,
                       allowParallel = T,summaryFunction = prSummary, savePredictions = T)

rf.1 = caret::train(is.goal ~ .,
                    data = train[,!colnames(train) %in% c("id")], 
                    method = "rf",
                    metric = "F",
                    trControl = control,
                    tuneGrid = grid,
                    preProcess = c("center", "scale"))

xG_test.rf.v1 = predict(rf.1, test, type = "prob")


#Repeating the process with additional variables
flag_vars = c('dribble.flag', 'tb.flag', 'dribble.to.shot.time', 'tb.to.shot.time')

shots_valid[flag_vars][is.na(shots_valid[flag_vars])] = 5000

#choosing independent variables
ind.vars = c('id', 'is.goal','distance.to.gk', 'distance.to.goal', 'angle.to.gk', 'angle.to.goal', 'gk.to.goalline', 'play_pattern.name','shot.body_part.name', 'shot.type.name', 'shot.technique.name', 'dribble.to.shot.time', 'tb.to.shot.time', 'TimeInPoss')

shots.varsdata = subset(shots_valid, select = ind.vars) %>% drop_na()

train = shots.varsdata[idx,]
test = shots.varsdata[-idx,]

#model 2

vars = ncol(model.matrix(is.goal ~ ., train[,!colnames(train) %in% c("id")])) - 2
grid = expand.grid(mtry = c(4:vars))
control = trainControl(classProbs = TRUE, method = "cv", number = 5,
                       allowParallel = T,summaryFunction = prSummary, savePredictions = T)
rf.2 = caret::train(is.goal ~ .,
                    data = train[,!colnames(train) %in% c("id")], 
                    method = "rf",
                    metric = "F",
                    trControl = control,
                    tuneGrid = grid,
                    preProcess = c("center", "scale"))
# tuneLength = 20


xG_test.rf.v2 = predict(rf.2, test, type = "prob")

#Building one ROC plot with data from all three

#building a dataframe to make a plot
roc.df = data.frame(is.goal = test$is.goal, rf.xg.v1 = xG_test.rf.v1$NoGoal, rf.xg.v2 = xG_test.rf.v2$NoGoal, sb.xg = 1 - test_xg)

roc.df = melt_roc(roc.df, 'is.goal', c('rf.xg.v1', 'rf.xg.v2', 'shot.statsbomb_xg'), names = c('RF V1','RF V2', 'SB'))

all.three_auc = ggplot(roc.df, aes(d = D, m = M, color = name)) + geom_roc(n.cuts = 0) + theme_tufte() + labs(x = "False Positive Rate", y = "True Positive Rate", color = "Model", title = "Comparing my models to SB's") + theme(plot.title = element_text(size=13,lineheight=.8, hjust = 0.5,vjust=1,family="serif"))

all.three_auc + annotate("text", x = 0.75, y = 0.35, label = paste("AUC(V1) = ", round(calc_auc(all.three_auc)$AUC[1], 2)), color = "#FF6600", family = "serif") + annotate("text", x = 0.75, y = 0.25, label = paste("AUC(V2) = ", round(calc_auc(all.three_auc)$AUC[2], 2)), color = "blue", family = "serif") + annotate("text", x = 0.75, y = 0.15, label = paste("AUC(SB) = ", round(calc_auc(all.three_auc)$AUC[3], 2)), color = "#333333", family = "serif") + scale_color_manual(values=c("#FF6600", "blue", "#333333")) 

#when stopping
stopCluster(cl)

#running the model on the full dataset

lm_eqn = function(y, x){
  m = lm(y ~ x);
  eq = substitute(italic(r)^2~"="~r2, 
                  list(r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}

shots.varsdata$my.xG = predict(rf.1, newdata = shots.varsdata, type = "prob")

shots_valid = left_join(shots_valid, shots.varsdata[,c("id", "my.xG")], by = "id")
shots_valid$my.xG = shots_valid$my.xG$Goal
shots_valid = shots_valid %>% drop_na(my.xG)

#creating correlation plots

#half time correlation

half.cor = shots_valid %>% filter(period == 1) %>%
  group_by(match_id) %>%
  summarise(goal.total = sum(is.goal.num, na.rm = T),
            xG.total = sum(my.xG, na.rm = T))

half.cor.sp = ggplot(half.cor, aes(goal.total, xG.total)) + geom_point() + geom_smooth(method="lm", se = F)
#+ labs(x = "foot length (cm)", y = "height (cm)") + geom_smooth(method="lm")
half.cor.sp + annotate("text", x = 3.5, y = 0.5, label = lm_eqn(half.cor$goal.total, half.cor$xG.total), parse = TRUE) + labs(title = "Goals vs xG at Half-Time", x = "Goals", y = "xG") + theme_tufte() + geom_rangeframe()+  theme(plot.title = element_text(size=13,lineheight=.8, hjust = 0.5,vjust=1,family="serif"))

#Full time correlation 
full.cor = shots_valid %>% group_by(match_id) %>%
  summarise(goal.total = sum(is.goal.num, na.rm = T),
            xG.total = sum(my.xG, na.rm = T))

full.cor.sp = ggplot(full.cor, aes(goal.total, xG.total)) + geom_point() + geom_smooth(method="lm", se = F)
#+ labs(x = "foot length (cm)", y = "height (cm)") + geom_smooth(method="lm")
full.cor.sp + annotate("text", x = 6.2, y = 1, label = lm_eqn(full.cor$goal.total, full.cor$xG.total), parse = TRUE) +  labs(title = "Goals vs xG at Full-Time", x = "Goals", y = "xG") + theme_tufte() + geom_rangeframe() +theme(plot.title = element_text(size=13,lineheight=.8, hjust = 0.5,vjust=1,family="serif"))

#correlation by goals in a gameweek
shots_valid = left_join(shots_valid, Matches[,c("match_id", "match_date")], by = "match_id")

shots_valid$match_week = week(shots_valid$match_date)
shots_valid$match_month = lubridate::month(shots_valid$match_date, label = T)

#weekly correlation 
week.cor = shots_valid %>% group_by(match_week) %>%
  summarise(goal.total = sum(is.goal.num, na.rm = T),
            xG.total = sum(my.xG, na.rm = T))

week.cor.sp = ggplot(week.cor, aes(goal.total, xG.total)) + geom_point() + geom_smooth(method="lm", se = F)
#+ labs(x = "foot length (cm)", y = "height (cm)") + geom_smooth(method="lm")

week.cor.sp + annotate("text", x = 20, y = 5, label = lm_eqn(week.cor$goal.total, week.cor$xG.total), parse = TRUE) +  labs(title = "Goals vs xG after a Week", x = "Goals", y = "xG") + geom_rangeframe()+ theme_tufte() +theme(plot.title = element_text(size=13,lineheight=.8, vjust=1,family="serif"))

#month correlation 
month.cor = shots_valid %>% group_by(match_month) %>%
  summarise(goal.total = sum(is.goal.num, na.rm = T),
            xG.total = sum(my.xG, na.rm = T))

month.cor.sp = ggplot(month.cor, aes(goal.total, xG.total)) + geom_point() + geom_smooth(method="lm", se = F)

month.cor.sp + annotate("text", x = 45, y = 15, label = lm_eqn(month.cor$goal.total, month.cor$xG.total), parse = TRUE) +  labs(title = "Goals vs xG after a Month", x = "Goals", y = "xG") + theme_tufte() + geom_rangeframe() +theme(plot.title = element_text(size=13,lineheight=.8, vjust=1,family="serif"))


#creating shots maps
pitch = create_StatsBomb_Pitch("#ffffff", "#A9A9A9", "#ffffff", "#000000", BasicFeatures=TRUE)

a1 = p + geom_raster(data = shots_valid, aes(x = location.x, y = location.y, fill = my.xG)) + scale_fill_gradient(low = "blue", high = "red") + labs(fill = "xG", title = "Shots with my xG") + theme_void() + theme(plot.title = element_text(size=13,lineheight=.8, vjust=1,family="serif"))

a2 = p + geom_raster(data = shots_valid, aes(x = location.x, y = location.y, fill = shot.statsbomb_xg)) + scale_fill_gradient(low = "blue", high = "red") + labs(fill = "xG", title = "Shots with Statsbomb xG") + theme_void() + theme(plot.title = element_text(size=13,lineheight=.8, vjust=1,family="serif"))

