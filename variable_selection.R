# Code to demonstrate various types of variable selection in regression lm/glm/gam
#
# Date: December 15, 2016
#
# Author: Naumaan Nayyar

# Clear workspace
rm(list=ls())

# Import required libraries
library(MASS)  # library contains stepwise selection
library(leaps)  # library contains all-subsets selection

# Use longley dataset. Note that the dataset is only used as a demonstration. It is highly collinear.

# Demonstrate collinearity of dataset
pairs(longley, main="Longley Economic Data")

# Develop initial model
model1 <- lm(Employed~GNP+GNP.deflator+Unemployed+Armed.Forces+Population+Year, data=longley)

# Alternative way of developing model1. Useful for large datasets with irregular naming. To omit an independent variable, use '-'
model1 <- lm(Employed~., data=longley)
print(summary(model1))

# 1. and 2. Stepwise variable selection - backward and both (forward+backward)
model1.stepbackward <- stepAIC(model1, direction="backward", trace=0)
print(summary(model1.stepbackward))
model1.stepboth <- stepAIC(model1, direction="both", trace=0)
print(summary(model1.stepboth))

# 3. Stepwise variable selection - forward
model1temp <- lm(Employed~GNP, data=longley)  # establish base model
model1.stepforward <- stepAIC(model1temp, scope=~.+GNP.deflator+Unemployed+Armed.Forces+Population+Year, direction="forward", trace=0)

# 3. Alternative method to generate model1.stepforward
model1.stepforward <- stepAIC(model1temp, scope=list(upper=model1, lower=model1temp), direction="forward", trace=0)
print(summary(model1.stepforward))

# 4. All-subsets selection - exhaustive model selection method
model1.allsubsets <- regsubsets(Employed~GNP+GNP.deflator+Unemployed+Armed.Forces+Population+Year, data=longley, method="exhaustive", nbest=2)  # select 2 best models for each predictor set size
print(summary(model1.allsubsets))
plot(model1.allsubsets,scale="r2")  # plot each model ordered by R2 selection statistic
