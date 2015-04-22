## Exercises Chapter 3
library(AppliedPredictiveModeling)
library(caret)
# Samples from the JSTAT article
load("~/R Projects/AppliedPredictiveModeling/data/descr.RData")
load("~/R Projects/AppliedPredictiveModeling/data/mutagen.RData")

# 3.1
library(mlbench)
data(Glass)
View(Glass)

# Examine the predictor variables visually
library(ggplot2)
par(mfrow=c(4,2))

width <- 1.25*(max(Glass$RI) - min(Glass$RI))/10.0
hist(Glass$RI)
ggplot(data=Glass, aes(x=RI)) + 
  geom_histogram(binwidth=width, colour="black", fill="white")


compounds <- names(Glass[-length(Glass)])
plotOne <- function(data, name, index) {
  # data is a data.frame of predictors
  # name is the vector of names that describes the predictors
  # index is the numeric position of the predictor to pick
  variable <- data[, index]
  heading <- name[index]
  hist(variable, main=paste("Histogram of ", heading), xlab=heading)
}
