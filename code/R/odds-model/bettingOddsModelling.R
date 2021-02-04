#######################
#Modelling
#######################

library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)  
matchData <- readRDS("./rds/matchData.rds")

class(matchData$winner) <- "numeric"
matchData$winner[matchData$winner == 3] <- 5
matchData$winner[matchData$winner == 1] <- 3
matchData$winner[matchData$winner == 5] <- 1

#mean of each of the methods:
  #-without
  #- add
  #- cum
  #- shin
  #- power
  #mean(rm.na=T) -> still i have NaN problem

#####################
#Cumulative:
#####################
data <- matchData %>% select(dplyr::contains("cum")) %>% select(dplyr::ends_with("H"))
matchData$cumHomeAVG <- apply(data, 1, mean, na.rm=T)

data <- matchData %>% select(dplyr::contains("cum")) %>% select(dplyr::ends_with("D")) 
matchData$cumDrawAVG <- apply(data, 1, mean, na.rm=T) 

data <- matchData %>% select(dplyr::contains("cum")) %>% select(dplyr::ends_with("A"))
matchData$cumAwayAVG <- apply(data, 1, mean, na.rm=T)
  
#####################
#Add:
#####################
data <- matchData %>% select(dplyr::contains("add")) %>% select(dplyr::ends_with("H"))
matchData$addHomeAVG <- apply(data, 1, mean, na.rm=T)

data <- matchData %>% select(dplyr::contains("add")) %>% select(dplyr::ends_with("D")) 
matchData$addDrawAVG <- apply(data, 1, mean, na.rm=T) 

data <- matchData %>% select(dplyr::contains("add")) %>% select(dplyr::ends_with("A"))
matchData$addAwayAVG <- apply(data, 1, mean, na.rm=T)

#####################
#Power:
#####################
data <- matchData %>% select(dplyr::contains("power")) %>% select(ends_with("H"))
matchData$powerHomeAVG <- apply(data, 1, mean, na.rm=T)

data <- matchData %>% select(dplyr::contains("power")) %>% select(dplyr::ends_with("D")) 
matchData$powerDrawAVG <- apply(data, 1, mean, na.rm=T) 

data <- matchData %>% select(contains("power")) %>% select(dplyr::ends_with("A"))
matchData$powerAwayAVG <- apply(data, 1, mean, na.rm=T)

#####################
#Shin:
#####################
data <- matchData %>% select(dplyr::contains("shin")) %>% select(dplyr::ends_with("H"))
matchData$shinHomeAVG <- apply(data, 1, mean, na.rm=T)

data <- matchData %>% select(dplyr::contains("shin")) %>% select(dplyr::ends_with("D")) 
matchData$shinDrawAVG <- apply(data, 1, mean, na.rm=T) 

data <- matchData %>% select(dplyr::contains("shin")) %>% select(dplyr::ends_with("A"))
matchData$shinAwayAVG <- apply(data, 1, mean, na.rm=T)


#####################
#RPS...
#####################
library("verification")
library(magrittr) 
matchData
rpf <- matchData %>% dplyr::select(dplyr::ends_with("AVG"))
rpf$winner <- matchData$winner
is.nan.data.frame <- function(x) {do.call(cbind, lapply(x, is.nan))}

matchData[is.nan(matchData)] <- NA
rpf$RPSAdd <- 0
rpf$RPSCum <- 0
rpf$RPSShin <- 0
rpf$RPSPower <- 0
for(i in names(rpf)){
  if(i == "winner"){
    for(j in 1:length(rpf[["winner"]])){
      rpf[["RPSAdd"]][j]  <- rps(obs = matrix(c(rpf[["winner"]][j])), pred = matrix(c(rpf[["addHomeAVG"]][j], rpf[["addDrawAVG"]][j],  rpf[["addAwayAVG"]][j]), nrow=1))[1]
      rpf[["RPSCum"]][j]  <- rps(obs = matrix(c(rpf[["winner"]][j])), pred = matrix(c(rpf[["cumHomeAVG"]][j], rpf[["cumDrawAVG"]][j],  rpf[["cumAwayAVG"]][j]), nrow=1))[1]
      rpf[["RPSShin"]][j]  <- rps(obs = matrix(c(rpf[["winner"]][j])), pred = matrix(c(rpf[["shinHomeAVG"]][j], rpf[["shinDrawAVG"]][j],  rpf[["shinAwayAVG"]][j]), nrow=1))[1]
      rpf[["RPSPower"]][j]  <- rps(obs = matrix(c(rpf[["winner"]][j])), pred = matrix(c(rpf[["powerHomeAVG"]][j], rpf[["powerDrawAVG"]][j],  rpf[["powerAwayAVG"]][j]), nrow=1))[1]
      
    }
  }
  
}

rpf$RPSAdd <- unlist(rpf$RPSAdd)
rpf$RPSCum <- unlist(rpf$RPSCum)
rpf$RPSShin <- unlist(rpf$RPSShin)
rpf$RPSPower <- unlist(rpf$RPSPower)


max(rpf$RPSAdd)
max(rpf$RPSCum)
max(rpf$RPSShin)
max(rpf$RPSPower)
