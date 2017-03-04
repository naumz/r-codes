# Demonstrating potentially misleading data science case studies
#
# Date: March 3, 2017
#
# Author: Naumaan Nayyar

#### ENVIRONMENT ####
# clear workspace
rm(list = ls())
cat("\014")
graphics.off()

# load required libraries
library(ggplot2)
library(bit64)
library(data.table)

#### Population size and its effect of best performance ####
# How different population sizes in different groups can mislead to the true
# nature of the underlyinng distribution
x <- sapply(1:6, function(x) 10^x)  # different population sizes
y <- lapply(x, function(x) max(rnorm(x)))  # finding best performer under the same distribution
plot(log(x), y, main = "Best performance vs Population size",
     xlab = "Log of population size", ylab = "Best Performer")

#### Simpson's Paradox ####
# How pooling cases from different groups can lead to incorrect conclusions
# source: UC Berkely gender bias
graduate.admits <- data.table(department = c("a","b","c","d","e","f"),
                             men.applicants = c(825, 560, 325, 417, 191, 373),
                             men.admit = c(0.62, 0.63, 0.37, 0.33, 0.28, 0.06),
                             women.applicants = c(108, 25, 593, 375, 393, 341),
                             women.admit = c(0.82, 0.68, 0.34, 0.35, 0.24, 0.07))
graduate.admits[, women.more.by := women.admit - men.admit]
overall.men.admit <- graduate.admits[, sum(men.applicants*men.admit)/sum(men.applicants)]
overall.women.admit <- graduate.admits[, sum(women.applicants*women.admit)/sum(women.applicants)]
overall.women.more.by = overall.women.admit - overall.men.admit  # lesser!
