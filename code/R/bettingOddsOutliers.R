load.image("./rds/bettingOdds.RData")
readRDS("./rds/matchData.rds")
readRDS("./rds/miceOutput.rds")
readRDS("./rds/knnImputedData.rds")
readRDS("./rds/pcaOutput.rds")

####################
#Outliers
####################
#we already deleted those rows that don't have data per all matches.
#but, do we have other outliers aside from that?

#boxplots


########
#LOF
########

valueseMiceLOF <- lofactor(miceOutput[7:36], k=5)
valueseKNNLOF <- lofactor(knnImputedData[7:36], k=5)
valuesePCALOF <- lofactor(pcaOutput[7:36], k=5) 



#######
#IQR
#######

iqrFilter <- function (x){
  Q1 = summary(x)[2]
  Q3 = summary(x)[3] 
  IQR = Q3 - Q1 
  filter = (x >= Q1 - 1.5 * IQR) & (x <= Q3 + 1.5 *IQR)
  return(x[filter])
}

iqrOutliers <- sapply(matchData[7:36], iqrFilter)



