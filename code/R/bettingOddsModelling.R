#bettingOddsModelling.R

#######################
# Data Model 2:
#######################
data2 <- results
data2$date <- as.Date(data2$date)
data2 <- data2 %>%
  select(-matches("*(power|add|cum|shin)"))


data2 <- data2[rowSums(is.na(data2[9:38])) != ncol(data2[9:38]), ]

matrixplot(data2 %>% select(-matches("*(B365|BW|IW|LB|PS|WH|SJ|VC|GB|BS)(A|D)")))

table(is.na(data2$SJH))

#######################
#Approach #1: mice approach
#######################
data2Mice <- data2[4:29]

miceImputatedData <- mice(data2Mice,m=5,maxit=5,meth='pmm',seed=500)

miceImputatedData$data
#Values obtained of the imputations:
summary(miceImputatedData)

#miceImputatedData$imp$Rent

#Visualizations:
#densityplot(miceImputatedData)
#stripplot(miceImputatedData)

#######################
#Approach #2: KNN imputation.
#######################
data2KNN <- data2
knnImputedData <- knnImputation(data = data2[4:29], k = 5, scale = T, meth="weighAvg")
summary(knnImputedData)

#######################
#Approach #3: Random Forest imputation.
#######################
data2RF <- data2
randomForestImputed <- missForest(data2RF[4:29])$ximp #takes way too much wtf

#######################
#Approach #4: imputePCA.
#######################
data2PCA <- data2
pcaImputed <- imputePCA(data2PCA[4:29], ncp=4)
pcaImputed <- as.data.frame(pcaImputed$completeObs)