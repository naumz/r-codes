# Various linear and non-linear classification algorithms
# Performance tested on IRIS dataset
#
# Date: January 14, 2017
#
# Author: Naumaan Nayyar

# clear workspace
rm(list=ls())

# load required libraries
library(matrixStats)  # count()
library(VGAM)  # Multivariate logistic regression
library(MASS)  # LDA, QDA
library(caret)  # PS-LDA, kNN
library(mda)  # MDA, FDA
library(nnet)  # Neural Networks
library(klaR)  # Regularized Discriminant Analysis
library(kernlab)  # SVM
library(e1071)  # naive Bayes
library(rpart)  # CART
library(RWeka)  # C4.5, PART
library(ipred)  # Bagging CART
library(randomForest)  # Random Forest
library(gbm)  # Gradient Boosted Machine
library(C50)  # Boosted C5.0

classLinReg <- function(mydata) {
  # Classifies through linear regression with indicator matrix
  # 
  # Args:
  #   mydata: data frame with the last column being the class of the row (factor)
  # Returns:
  #   The proportion of the data successfully fit.
  x <- as.matrix(mydata[1:(ncol(mydata)-1)]) # getting feature matrix
  x <- cbind(x, rep(1, nrow(mydata))) # appending row of 1's for regression
  y.name <- as.name(tail(names(mydata), n=1))  # get name of label of class
  y <- model.matrix( ~ eval(y.name) - 1, data = mydata)  # generating indicator
  # matrix, -1 used to ignore intercept term, eval(as.name) used to put
  # variable name in formula.
  actual.labels <- mydata[,ncol(mydata)]
  # perform linear regression classification
  b <- solve(t(x) %*% x) %*% t(x) %*% y
  fit <- x %*% b
  fit.labels <- max.col(fit)  # find which column has max value (can also use apply + which.max)
  for (i in 1:nlevels(mydata[[paste(y.name)]])) {
  	fit.labels[which(fit.labels==paste(i))] <- levels(mydata[[paste(y.name)]])[i]
  }
  # compute accuracy of model (can overfit if data size is small)
  accuracy.model <- count(fit.labels == actual.labels)/nrow(mydata)
  accuracy.table <- table(fit.labels, actual.labels)
  print(paste("Fitted labels vs actual labels - ", "Linear Regression Indicator Matrix Classification"))
  print(accuracy.table)
  return(accuracy.table)
}

classLogistic <- function(mydata) {
  # Classifies through logistic regression
  # 
  # Args:
  #   mydata: data frame with the last column being the class of the row (factor)
  # Returns:
  #   The proportion of the data successfully fit.
  y.name <- as.name(tail(names(mydata), n=1))  # get name of label of class
  # variable name in formula.
  actual.labels <- mydata[,ncol(mydata)]
  # generate formula and model
  fmla <- as.formula(paste(y.name, " ~ ."))
  model <- vglm(fmla, family=multinomial, data=mydata)
  fit <- predict(model, mydata[, 1:(ncol(mydata)-1)], type="response")
  fit.labels <- max.col(fit)  # find which column has max value (can also use apply + which.max)
  for (i in 1:nlevels(mydata[[paste(y.name)]])) {
  	fit.labels[which(fit.labels==paste(i))] <- levels(mydata[[paste(y.name)]])[i]
  }
  # compute accuracy of model (can overfit if data size is small)
  accuracy.model <- count(fit.labels == actual.labels)/nrow(mydata)
  accuracy.table <- table(fit.labels, actual.labels)
  print(paste("Fitted labels vs actual labels - ", "Logistic Regression"))
  print(accuracy.table)
  return(accuracy.table)
}

classLDA <- function(mydata) {
  # Classifies through Linear Discriminant Analysis
  # 
  # Args:
  #   mydata: data frame with the last column being the class of the row (factor)
  # Returns:
  #   The proportion of the data successfully fit.
  y.name <- as.name(tail(names(mydata), n=1))  # get name of label of class
  # variable name in formula.
  actual.labels <- mydata[,ncol(mydata)]
  # generate formula and model
  fmla <- as.formula(paste(y.name, " ~ ."))
  model <- lda(fmla, data=mydata)
  fit.labels <- predict(model, mydata[, 1:(ncol(mydata)-1)])$class
  # compute accuracy of model (can overfit if data size is small)
  accuracy.model <- count(fit.labels == actual.labels)/nrow(mydata)
  accuracy.table <- table(fit.labels, actual.labels)
  print(paste("Fitted labels vs actual labels - ", "Linear Discriminant Analysis"))
  print(accuracy.table)
  return(accuracy.table)
}

classPSLDA <- function(mydata) {
  # Classifies through Partial Least Squares Linear Discriminant Analysis
  # 
  # Args:
  #   mydata: data frame with the last column being the class of the row (factor)
  # Returns:
  #   The proportion of the data successfully fit.
  x <- mydata[1:(ncol(mydata)-1)]
  y <- mydata[,ncol(mydata)]
  # variable name in formula.
  actual.labels <- mydata[,ncol(mydata)]
  # generate model
  model <- plsda(x, y, probMethod="Bayes")
  fit.labels <- predict(model, mydata[,1:(ncol(mydata)-1)])
  # compute accuracy of model (can overfit if data size is small)
  accuracy.model <- count(fit.labels == actual.labels)/nrow(mydata)
  accuracy.table <- table(fit.labels, actual.labels)
  print(paste("Fitted labels vs actual labels - ", "Partial Least Squares Linear Discriminant Analysis"))
  print(accuracy.table)
  return(accuracy.table)
}

classMDA <- function(mydata) {
  # Classifies through Mixture Discriminant Analysis
  # 
  # Args:
  #   mydata: data frame with the last column being the class of the row (factor)
  # Returns:
  #   The proportion of the data successfully fit.
  y.name <- as.name(tail(names(mydata), n=1))  # get name of label of class
  # variable name in formula.
  actual.labels <- mydata[,ncol(mydata)]
  # generate formula and model
  fmla <- as.formula(paste(y.name, " ~ ."))
  model <- mda(fmla, data=mydata)
  fit.labels <- predict(model, mydata[, 1:(ncol(mydata)-1)])
  # compute accuracy of model (can overfit if data size is small)
  accuracy.model <- count(fit.labels == actual.labels)/nrow(mydata)
  accuracy.table <- table(fit.labels, actual.labels)
  print(paste("Fitted labels vs actual labels - ", "Mixture Discriminant Analysis"))
  print(accuracy.table)
  return(accuracy.table)
}

classQDA <- function(mydata) {
  # Classifies through Quadratic Discriminant Analysis
  # 
  # Args:
  #   mydata: data frame with the last column being the class of the row (factor)
  # Returns:
  #   The proportion of the data successfully fit.
  y.name <- as.name(tail(names(mydata), n=1))  # get name of label of class
  # variable name in formula.
  actual.labels <- mydata[,ncol(mydata)]
  # generate formula and model
  fmla <- as.formula(paste(y.name, " ~ ."))
  model <- qda(fmla, data=mydata)
  fit.labels <- predict(model, mydata[, 1:(ncol(mydata)-1)])$class
  # compute accuracy of model (can overfit if data size is small)
  accuracy.model <- count(fit.labels == actual.labels)/nrow(mydata)
  accuracy.table <- table(fit.labels, actual.labels)
  print(paste("Fitted labels vs actual labels - ", "Quadratic Discriminant Analysis"))
  print(accuracy.table)
  return(accuracy.table)
}

classRDA <- function(mydata) {
  # Classifies through Regularized Discriminant Analysis
  # 
  # Args:
  #   mydata: data frame with the last column being the class of the row (factor)
  # Returns:
  #   The proportion of the data successfully fit.
  y.name <- as.name(tail(names(mydata), n=1))  # get name of label of class
  # variable name in formula.
  actual.labels <- mydata[,ncol(mydata)]
  # generate formula and model
  fmla <- as.formula(paste(y.name, " ~ ."))
  model <- qda(fmla, data=mydata, gamma=0.05, lambda=0.01)
  fit.labels <- predict(model, mydata[, 1:(ncol(mydata)-1)])$class
  # compute accuracy of model (can overfit if data size is small)
  accuracy.model <- count(fit.labels == actual.labels)/nrow(mydata)
  accuracy.table <- table(fit.labels, actual.labels)
  print(paste("Fitted labels vs actual labels - ", "Regularized Discriminant Analysis"))
  print(accuracy.table)
  return(accuracy.table)
}

classNN <- function(mydata) {
  # Classifies through Neural Network
  # 
  # Args:
  #   mydata: data frame with the last column being the class of the row (factor)
  # Returns:
  #   The proportion of the data successfully fit.
  y.name <- as.name(tail(names(mydata), n=1))  # get name of label of class
  # variable name in formula.
  actual.labels <- mydata[,ncol(mydata)]
  # generate formula and model
  fmla <- as.formula(paste(y.name, " ~ ."))
  model <- nnet(fmla, data=mydata, size=4, decay=0.0001, maxit=500)
  fit.labels <- predict(model, mydata[, 1:(ncol(mydata)-1)], type="class")
  # compute accuracy of model (can overfit if data size is small)
  accuracy.model <- count(fit.labels == actual.labels)/nrow(mydata)
  accuracy.table <- table(fit.labels, actual.labels)
  print(paste("Fitted labels vs actual labels - ", "Neural Network"))
  print(accuracy.table)
  return(accuracy.table)
}

classFDA <- function(mydata) {
  # Classifies through Flexible Discriminant Analysis
  # 
  # Args:
  #   mydata: data frame with the last column being the class of the row (factor)
  # Returns:
  #   The proportion of the data successfully fit.
  y.name <- as.name(tail(names(mydata), n=1))  # get name of label of class
  # variable name in formula.
  actual.labels <- mydata[,ncol(mydata)]
  # generate formula and model
  fmla <- as.formula(paste(y.name, " ~ ."))
  model <- fda(fmla, data=mydata)
  fit.labels <- predict(model, mydata[, 1:(ncol(mydata)-1)])
  # compute accuracy of model (can overfit if data size is small)
  accuracy.model <- count(fit.labels == actual.labels)/nrow(mydata)
  accuracy.table <- table(fit.labels, actual.labels)
  print(paste("Fitted labels vs actual labels - ", "Flexible Discriminant Analysis"))
  print(accuracy.table)
  return(accuracy.table)
}

classSVM <- function(mydata) {
  # Classifies through Support Vector Machine
  # 
  # Args:
  #   mydata: data frame with the last column being the class of the row (factor)
  # Returns:
  #   The proportion of the data successfully fit.
  y.name <- as.name(tail(names(mydata), n=1))  # get name of label of class
  # variable name in formula.
  actual.labels <- mydata[,ncol(mydata)]
  # generate formula and model
  fmla <- as.formula(paste(y.name, " ~ ."))
  model <- ksvm(fmla, data=mydata)
  fit.labels <- predict(model, mydata[, 1:(ncol(mydata)-1)], type="response")
  # compute accuracy of model (can overfit if data size is small)
  accuracy.model <- count(fit.labels == actual.labels)/nrow(mydata)
  accuracy.table <- table(fit.labels, actual.labels)
  print(paste("Fitted labels vs actual labels - ", "Support Vector Machine"))
  print(accuracy.table)
  return(accuracy.table)
}

classKNN <- function(mydata) {
  # Classifies through k-Nearest Neighbors
  # 
  # Args:
  #   mydata: data frame with the last column being the class of the row (factor)
  # Returns:
  #   The proportion of the data successfully fit.
  y.name <- as.name(tail(names(mydata), n=1))  # get name of label of class
  # variable name in formula.
  actual.labels <- mydata[,ncol(mydata)]
  # generate formula and model
  fmla <- as.formula(paste(y.name, " ~ ."))
  model <- knn3(fmla, data=mydata, k=5)
  fit.labels <- predict(model, mydata[, 1:(ncol(mydata)-1)], type="class")
  # compute accuracy of model (can overfit if data size is small)
  accuracy.model <- count(fit.labels == actual.labels)/nrow(mydata)
  accuracy.table <- table(fit.labels, actual.labels)
  print(paste("Fitted labels vs actual labels - ", "k-Nearest Neighbors"))
  print(accuracy.table)
  return(accuracy.table)
}

classNB <- function(mydata) {
  # Classifies through naive Bayes
  # 
  # Args:
  #   mydata: data frame with the last column being the class of the row (factor)
  # Returns:
  #   The proportion of the data successfully fit.
  y.name <- as.name(tail(names(mydata), n=1))  # get name of label of class
  # variable name in formula.
  actual.labels <- mydata[,ncol(mydata)]
  # generate formula and model
  fmla <- as.formula(paste(y.name, " ~ ."))
  model <- naiveBayes(fmla, data=mydata)
  fit.labels <- predict(model, mydata[, 1:(ncol(mydata)-1)])
  # compute accuracy of model (can overfit if data size is small)
  accuracy.model <- count(fit.labels == actual.labels)/nrow(mydata)
  accuracy.table <- table(fit.labels, actual.labels)
  print(paste("Fitted labels vs actual labels - ", "naive Bayes"))
  print(accuracy.table)
  return(accuracy.table)
}

classCART <- function(mydata) {
  # Classifies through Classification and Regression Trees
  # 
  # Args:
  #   mydata: data frame with the last column being the class of the row (factor)
  # Returns:
  #   The proportion of the data successfully fit.
  y.name <- as.name(tail(names(mydata), n=1))  # get name of label of class
  # variable name in formula.
  actual.labels <- mydata[,ncol(mydata)]
  # generate formula and model
  fmla <- as.formula(paste(y.name, " ~ ."))
  model <- rpart(fmla, data=mydata)
  fit.labels <- predict(model, mydata[, 1:(ncol(mydata)-1)], type="class")
  # compute accuracy of model (can overfit if data size is small)
  accuracy.model <- count(fit.labels == actual.labels)/nrow(mydata)
  accuracy.table <- table(fit.labels, actual.labels)
  print(paste("Fitted labels vs actual labels - ", "Classification and Regression Trees"))
  print(accuracy.table)
  return(accuracy.table)
}

classC45 <- function(mydata) {
  # Classifies through C4.5
  # 
  # Args:
  #   mydata: data frame with the last column being the class of the row (factor)
  # Returns:
  #   The proportion of the data successfully fit.
  y.name <- as.name(tail(names(mydata), n=1))  # get name of label of class
  # variable name in formula.
  actual.labels <- mydata[,ncol(mydata)]
  # generate formula and model
  fmla <- as.formula(paste(y.name, " ~ ."))
  model <- J48(fmla, data=mydata)
  fit.labels <- predict(model, mydata[, 1:(ncol(mydata)-1)])
  # compute accuracy of model (can overfit if data size is small)
  accuracy.model <- count(fit.labels == actual.labels)/nrow(mydata)
  accuracy.table <- table(fit.labels, actual.labels)
  print(paste("Fitted labels vs actual labels - ", "C4.5"))
  print(paste(accuracy.table))
  return(accuracy.table)
}

classPART <- function(mydata) {
  # Classifies through repeated pruning C4.5
  # 
  # Args:
  #   mydata: data frame with the last column being the class of the row (factor)
  # Returns:
  #   The proportion of the data successfully fit.
  y.name <- as.name(tail(names(mydata), n=1))  # get name of label of class
  # variable name in formula.
  actual.labels <- mydata[,ncol(mydata)]
  # generate formula and model
  fmla <- as.formula(paste(y.name, " ~ ."))
  model <- PART(fmla, data=mydata)
  fit.labels <- predict(model, mydata[, 1:(ncol(mydata)-1)])
  # compute accuracy of model (can overfit if data size is small)
  accuracy.model <- count(fit.labels == actual.labels)/nrow(mydata)
  accuracy.table <- table(fit.labels, actual.labels)
  print(paste("Fitted labels vs actual labels - ", "Repeated pruning C4.5"))
  print(accuracy.table)
  return(accuracy.table)
}

classBCART <- function(mydata) {
  # Classifies through Bagging Classification and Regression Trees
  # 
  # Args:
  #   mydata: data frame with the last column being the class of the row (factor)
  # Returns:
  #   The proportion of the data successfully fit.
  y.name <- as.name(tail(names(mydata), n=1))  # get name of label of class
  # variable name in formula.
  actual.labels <- mydata[,ncol(mydata)]
  # generate formula and model
  fmla <- as.formula(paste(y.name, " ~ ."))
  model <- bagging(fmla, data=mydata)
  fit.labels <- predict(model, mydata[, 1:(ncol(mydata)-1)], type="class")
  # compute accuracy of model (can overfit if data size is small)
  accuracy.model <- count(fit.labels == actual.labels)/nrow(mydata)
  accuracy.table <- table(fit.labels, actual.labels)
  print(paste("Fitted labels vs actual labels - ", "Bagging Classification and Regression Trees"))
  print(accuracy.table)
  return(accuracy.table)
}

classRF <- function(mydata) {
  # Classifies through Random Forest
  # 
  # Args:
  #   mydata: data frame with the last column being the class of the row (factor)
  # Returns:
  #   The proportion of the data successfully fit.
  y.name <- as.name(tail(names(mydata), n=1))  # get name of label of class
  # variable name in formula.
  actual.labels <- mydata[,ncol(mydata)]
  # generate formula and model
  fmla <- as.formula(paste(y.name, " ~ ."))
  model <- randomForest(fmla, data=mydata)
  fit.labels <- predict(model, mydata[, 1:(ncol(mydata)-1)])
  # compute accuracy of model (can overfit if data size is small)
  accuracy.model <- count(fit.labels == actual.labels)/nrow(mydata)
  accuracy.table <- table(fit.labels, actual.labels)
  print(paste("Fitted labels vs actual labels - ", "Random Forest"))
  print(accuracy.table)
  return(accuracy.table)
}

classGBM <- function(mydata) {
  # Classifies through Gradient Boosted Machine
  # 
  # Args:
  #   mydata: data frame with the last column being the class of the row (factor)
  # Returns:
  #   The proportion of the data successfully fit.
  y.name <- as.name(tail(names(mydata), n=1))  # get name of label of class
  # variable name in formula.
  actual.labels <- mydata[,ncol(mydata)]
  # generate formula and model
  fmla <- as.formula(paste(y.name, " ~ ."))
  model <- gbm(fmla, data=mydata, distribution="multinomial", n.trees=100)
  fit <- predict(model, mydata, n.trees=model$n.trees, type="link")[,,1]
  fit.labels <- max.col(fit)
  for (i in 1:nlevels(mydata[[paste(y.name)]])) {
  	fit.labels[which(fit.labels==paste(i))] <- levels(mydata[[paste(y.name)]])[i]
  }
  # compute accuracy of model (can overfit if data size is small)
  accuracy.model <- count(fit.labels == actual.labels)/nrow(mydata)
  accuracy.table <- table(fit.labels, actual.labels)
  print(paste("Fitted labels vs actual labels - ", "Gradient Boosted Machine"))
  print(accuracy.table)
  return(accuracy.table)
}

classBC5 <- function(mydata) {
  # Classifies through Boosted C5.0
  # 
  # Args:
  #   mydata: data frame with the last column being the class of the row (factor)
  # Returns:
  #   The proportion of the data successfully fit.
  y.name <- as.name(tail(names(mydata), n=1))  # get name of label of class
  # variable name in formula.
  actual.labels <- mydata[,ncol(mydata)]
  # generate formula and model
  fmla <- as.formula(paste(y.name, " ~ ."))
  model <- C5.0(fmla, data=mydata, trials=10)
  fit.labels <- predict(model, mydata[, 1:(ncol(mydata)-1)])
  # compute accuracy of model (can overfit if data size is small)
  accuracy.model <- count(fit.labels == actual.labels)/nrow(mydata)
  accuracy.table <- table(fit.labels, actual.labels)
  print(paste("Fitted labels vs actual labels - ", "Boosted C5.0"))
  print(accuracy.table)
  return(accuracy.table)
}

# data(iris)
# mydata <- iris

wine.data <- read.csv('Datasets/wine.data.txt', header=FALSE)
wine.data <- wine.data[,c(seq(2,14),1)]
wine.data$V1 <- as.factor(wine.data$V1)
mydata <- wine.data

# Linear classification models
model1 <- classLinReg(mydata)
model2 <- classLogistic(mydata)
model3 <- classLDA(mydata)
model4 <- classPSLDA(mydata)

# Non-linear classification models
model5 <- classMDA(mydata)
model6 <- classQDA(mydata)
model7 <- classRDA(mydata)
model8 <- classNN(mydata)
model9 <- classFDA(mydata)
model10 <- classSVM(mydata)
model11 <- classKNN(mydata)
model12 <- classNB(mydata)

# Non-linear classification decision tree models
model13 <- classCART(mydata)
model14 <- classC45(mydata)
model15 <- classPART(mydata)
model16 <- classBCART(mydata)
model17 <- classRF(mydata)
model18 <- classGBM(mydata)
model19 <- classBC5(mydata)
