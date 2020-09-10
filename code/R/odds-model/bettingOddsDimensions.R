load.image("bettingOdds.RData")
readRDS("matchData.rds")
readRDS("miceOutput.rds")
readRDS("knnImputedData.rds")
readRDS("pcaOutput.rds")




####################
#Correlations
####################


correlationPlot <- function(x){
  varRegex <- NULL
  ifelse(x == "H",varRegex <- "(A|D)",ifelse(x=="A", varRegex <- "(H|D)", ifelse(x=="D",varRegex <- "(H|A)", varRegex <- "2")))
  correlationDataWins <- matchData %>% select(-matches(paste0("*(B365|BW|IW|LB|PS|WH|SJ|VC|GB|BS)",varRegex)))
  remove <- c("season", "stage","date","match_api_id","home_team_api_id", "away_team_api_id", "league_name", "winner")
  correlationDataWins <- correlationDataWins %>% select(-one_of(remove))
  pmat <- cor_pmat(correlationDataWins)
  corr <- round(cor(correlationDataWins,use = "complete.obs"), 1)
  ggcorrplot(corr, type = "lower", p.mat = pmat)
    
}

correlationPlot("H")
correlationPlot("A")
correlationPlot("D")
correlationPlot("ALL")

trend <- as.data.frame(list(matchData$season, matchData$league_name, matchData$winner))
tableplot(trend, sortCol = y)


####################
#PCA
####################fa
remove <- c("season", "stage","date","match_api_id","home_team_api_id", "away_team_api_id", "league_name", "winner")

pcaData <- miceOutput %>% select(-one_of(remove))
pcaData <- miceOutput
pca.data <- prcomp(pcaData, center = T,scale. = T)

#segurament està així perquè és el invers... però aquí dóna a entendre el contrari.
ggbiplot(pca.data, group=pcaData$winner, obs.scale = 1, alpha = 0.2, varname.size = 4, varname.abbrev = T) +
  scale_color_manual(name="winner", values=c("red", "green", "blue")) +
  theme(legend.direction ="horizontal", 
        legend.position = "bottom")


####################
#ANOVA
####################
data(decathlon, package="FactoMineR")
#100m
class(decathlon$`100m`)
class(decathlon$Competition)
data = data.frame(x1=miceOutput$stage, x2=miceOutput$season)
AnovaModel.1 <- aov(x1 ~ x2, data=data)
summary(AnovaModel.1)

#Assumptions
dwtest(AnovaModel.1, alternative ="two.sided")
shapiro.test(residuals(AnovaModel.1))
lmtest::bptest(AnovaModel.1)
# The p value that Anova gives us is less than 0.05, it means that we reject null hypothesis, so these distributions are different.


#overround model:

#AIC
#BIC
