#######################
#Modelling
#######################
load.image("bettingOdds.RData")
readRDS("matchData.rds")
readRDS("miceOutput.rds")
readRDS("knnImputedData.rds")
readRDS("pcaOutput.rds")
#######################
#Naive Bayes
#######################

x <- cbind(x_train,y_train)
# Fitting model
fit <-naiveBayes(y_train ~ ., data = x)
summary(fit)
#Predict Output 
predicted= predict(fit,x_test)
#######################
#KNN: (we need to normalize data)
#######################

x <- cbind(x_train,y_train)
# Fitting model
fit <-knn(y_train ~ ., data = x,k=5)
summary(fit)
#Predict Output 
predicted= predict(fit,x_test)


#######################
#SVM's
#######################

x <- cbind(x_train,y_train)
# Fitting model
fit <-svm(y_train ~ ., data = x)
summary(fit)
#Predict Output 
predicted= predict(fit,x_test)


#######################
#XGBoost
#######################


#######################
#Trees
#######################

#######################
#QDA
#######################

