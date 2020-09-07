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
####################
remove <- c("season", "stage","date","match_api_id","home_team_api_id", "away_team_api_id", "league_name", "winner")

pcaData <- matchData %>% select(-one_of(remove))
pca.data <- prcomp(na.omit(pcaData), center = TRUE,scale. = TRUE)
library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)
ggbiplot(pca.data, label=rownames(pca.data))
