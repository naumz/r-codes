# Example code to demonstrate how to deal with multiple variables together
#
# Date: December 16, 2016
#
# Author: Naumaan Nayyar

# Clear workspace
rm(list=ls())

# Import required libraries
library(MASS)
library(car)

# import data from csv
mydata <- read.csv('Datasets/mlr06.csv')

# generate formula by handling variables together
xnam <- paste0("X", 2:7)
fmla <- as.formula(paste("X1 ~ ", paste(xnam, collapse= "+")))

# build lm model
model1 <- lm(fmla, data=mydata)
print(summary(model1))

# identify outliers (12, 38, 41, 48)
print(outlierTest(model1))
plot(model1,which=c(2,5))

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
