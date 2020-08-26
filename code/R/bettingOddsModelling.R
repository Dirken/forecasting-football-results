#bettingOddsModelling.R

#######################
# Data Model 1:
#######################
#omit all NA -> we get just too few cases 
#length(data1$match_api_id) = 2756 (10% of dataset with no missings at all)
data1 <- results
length(data1$match_api_id)
#discarded

#######################
# Data Model 2:
#######################
#omitting betting houses:
#PS, BS, GB and SJ (with a 57.0%, 45.5%, 45.5% and 34.2% respectively), we will save SJ

data2 <- results
data2 <- data2 %>%
  select(-matches("*(PS|BS|GB)(H|D|A)"))


data22 <- data2 %>%
  select(-matches("*(power|add|cum|shin)"))

data22$date <- as.Date(data22$date)

#######################
# Approach #1: regression (simple method)
#######################
data22Regression <- data22

prediction  <- lm(SJH ~ . , data = data22Regression)
data22Regression$SJH[is.na(data22Regression$SJH)] <- predict(prediction, data22Regression)[is.na(data22Regression$SJH)]
names(data22)
#######################
#Approach #2: mice approach
#######################
data22Mice <- data22[4:29]

miceImputatedData <- mice(data22Mice,m=5,maxit=5,meth='pmm',seed=500)

miceImputatedData$data
#Values obtained of the imputations:
summary(miceImputatedData)

#miceImputatedData$imp$Rent

#Visualizations:
#densityplot(miceImputatedData)
#stripplot(miceImputatedData)

#######################
#Approach #3: KNN imputation.
#######################
data22KNN <- data22
knnImputedData <- knnImputation(data = data22[4:29], k = 5, scale = T, meth="weighAvg")
summary(knnImputedData)

#######################
#Approach #4: Random Forest imputation.
#######################
data22RF <- data22
randomForestImputed <- missForest(data22RF[4:29])$ximp

#######################
#Approach #5: imputePCA.
#######################
data22PCA <- data22
pcaImputed <- imputePCA(data22PCA, ncp=4)
pcaImputed <- as.data.frame(pcaImputed$completeObs)
