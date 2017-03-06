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
# WHY?
# Because greater the sample size, more the probability of picking a larger
# number when the distribution is long-tailed.
x <- sapply(1:6, function(x) 10^x)  # different population sizes
y <- lapply(x, function(x) max(rnorm(x)))  # finding best performer under the same distribution
pop.plot <- qplot(x = x, y = unlist(y), geom = "point") +
  scale_x_log10() +
  labs(title = "Best performance vs Population size",
       x = "Population (sample) Size",
       y = "Best Performer") +
  geom_text(aes(label = round(unlist(y), 2)), nudge_y = 0.2)
print(pop.plot)  # best performer gets better as population size increases

#### Simpson's Paradox ####
# How pooling cases from different groups can lead to incorrect conclusions
# source: UC Berkely gender bias
# WHY?
# Beacuse weighted-averages depend on the size of the group weights.
graduate.admits <- data.table(department = c("a","b","c","d","e","f"),
                             men.applicants = c(825, 560, 325, 417, 191, 373),
                             men.admit = c(0.62, 0.63, 0.37, 0.33, 0.28, 0.06),
                             women.applicants = c(108, 25, 593, 375, 393, 341),
                             women.admit = c(0.82, 0.68, 0.34, 0.35, 0.24, 0.07))
melt.admit <- melt(graduate.admits, id = 1,
                    measure = patterns("admit"), variable.name = "type",
                    value.name = "admit.fraction", variable.factor = TRUE)
setattr(melt.admit$type, "levels", c("men", "women"))
melt.applicant <- melt(graduate.admits, id = 1,
                       measure = patterns("applicants"), variable.name = "type",
                       value.name = "applicant.number", variable.factor = TRUE)
setattr(melt.applicant$type, "levels", c("men", "women"))
graduate.admits.melt <- merge(melt.applicant, melt.admit)
graduate.admits.summary <- melt(graduate.admits.melt[,
                        .(mean.value = mean(admit.fraction),
                          weighted.mean.value = weighted.mean(admit.fraction, applicant.number)),
                        by=type], id = 1)
simpson.plot <- ggplot(data = graduate.admits.melt,
                       aes(x = type, y = admit.fraction)) +
                labs(title = "Admission fraction vs Gender",
                     x = "Gender", y = "Admit fraction") +
                geom_point(aes(size = applicant.number)) +
                geom_point(data = graduate.admits.summary,
                           aes(x = type, y = value, color = variable)) +
                geom_text(data = graduate.admits.summary,
                          aes(x = type, y = value, label = round(value, 2)),
                          nudge_x = 0.15, check_overlap = TRUE)
print(simpson.plot)  # weighted admit mean for men is greater than for women,
                     # unweighted admit mean for women is greater than for men 
