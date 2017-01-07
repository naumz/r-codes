# Example code to demonstrate how to deal with multiple variables together
# (includes outlier detection)
#
# Date: December 16, 2016
#
# Author: Naumaan Nayyar

# Clear workspace
rm(list=ls())

# Import required libraries
library(MASS)
library(car)  # for outliers
library(outliers)  # for outliers
library(mvoutlier)  # for outliers
library(DMwR)  # for outliers
library(mclust)  # for outliers
library(cluster)  # for outliers
library(fpc)  # for outliers

# import data from csv
mydata <- read.csv('Datasets/mlr06.csv')

# generate formula by handling variables together
xnam <- paste0("X", 2:7)
fmla <- as.formula(paste("X1 ~ ", paste(xnam, collapse= "+")))

# build lm model
model1 <- lm(fmla, data=mydata)
print(summary(model1))

# identify outliers (12, 38, 41, 48)
print(outlierTest(model1))  # method 1 - Bonferroni p-value method
plot(model1,which=c(2,5))  # method 2 - QQ, Residual, Cook
chisq.plot(mydata)  # method 3 - Robust Mahalanobis Distance
outlier <- sort(lofactor(mydata,25),index.return=TRUE)
b <- outlier$ix
outliers.LOF <- tail(b,3)  # method 4 - 3 outliers using Local Outlier Factor (k=25)
fit <-kmeans(mydata,4)
s<- sort(fit$size,index.return=TRUE)
clusplot(mydata, fit$cluster, cex=1, col.p=fit$cluster)
outliers.clu <- which(fit$cluster==s$ix[1])  # method 5 - k-means clustering (k=4)

# center independent variables about zero (fixes insignificant intercept issue etc.). Alternative to eval-parse code below is to use eval(as.name()) or get() with column numbers.
mydata.centered = data.frame(X1=mydata$X1)
varnames <- paste0("mydata$X", 2:7)
index.names <- paste0("X", 2:7)
for (i in 1:6){
	assign(index.names[i], eval(parse(text=varnames[i])) - mean(eval(parse(text=varnames[i]))))
	mydata.centered[index.names[i]] <- eval(as.name(index.names[i]))
}

# rebuild glm model with outliers removed
model1 <- lm(fmla, data=mydata.centered[-c(12,38,41,48),])
print(summary(model1))

# perform stepwise variable selection (controversial)
model1.stepboth <- stepAIC(model1, direction="both", trace=0)
print(summary(model1.stepboth))
