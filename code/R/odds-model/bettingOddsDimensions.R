load.image("bettingOdds.RData")
matchData <- readRDS("./rds/matchData-noOverround.rds")
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
  scale_color_manual(name="winner", values=c("red", "green", "blue")) +
  theme(legend.direction ="horizontal", 
        legend.position = "bottom")

matchData$winner <- as.factor(matchData$winner)
names(matchData)

nullModel <- glm(winner ~ 1, matchData, family="gaussian")
completeModel<- lm(winner ~ ., matchData)

forwardModel <- step(nullModel, 
                     scope = list(upper=completeModel), 
                     direction="both", criterion = "BIC", 
                     k=log(nrow(matchData)))

backwardModel <- step(completeModel, 
                      scope = list(lower=nullModel), 
                      direction="both", 
                      criterion = "BIC", 
                      k=log(nrow(matchData)))