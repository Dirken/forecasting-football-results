load.image("./rds/bettingOdds.RData")
matchData <- readRDS("./rds/matchData.rds")
miceOutput <- readRDS("./rds/miceOutput.rds")
knnImputatedData <- readRDS("./rds/knnImputedData.rds")
pcaOutput <- readRDS("./rds/pcaOutput.rds")

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




#we are not going to apply outlier detection, instead we are going to do a weighted average because as 
#it is mentioned in literature, taking the maximum is not worthy. Also non of this values is invented,
#all are real, therefore we are going to keep them and scale them. Luckily by doing average, they will
#not do weird thigns.



