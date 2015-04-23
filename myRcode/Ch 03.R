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

# Use base R plotting
plotOne <- function(data, name, index) {
  # data is a data.frame of predictors
  # name is the vector of names that describes the predictors
  # index is the numeric position of the predictor to pick
  variable <- data[, index]
  heading <- name[index]
  hist(variable, main=paste("Histogram of ", heading), sub="Mean in Red, Median in Blue", xlab=heading)
  abline(v=mean(variable), col="red")
  abline(v=median(variable), col="blue")
}

# Exercise the R base plotting utility function for the Glass dataset
compounds <- names(Glass[-length(Glass)])
i <- 1
while(i <= length(compounds)) {
  plotOne (Glass, compounds, i)
  i <- i+1
}

# # Use ggplot2
# library(ggplot2)
# 
# width <- 1.25*(max(Glass[2]) - min(Glass[2]))/10.0
# 
# ggplot(data=Glass, aes(x=compounds[2])) + 
#   geom_histogram(binwidth=width, colour="black", fill="white")

# Examine skewness
library(e1071)
skewValues <- apply(Glass[-length(Glass)], 2, skewness)
# Transform for skewness
library(caret)
# bcTrans is the transformation function (calculated lamdbas)
bcTrans <- apply(Glass[-length(Glass)], 2, BoxCoxTrans)
# We could follow this by centering, scaling, ... but
# the preProcess() function can perform all these transformations at once
# in the right order

# All Transformations at once
allTrans <- preProcess(Glass[-length(Glass)], method=c("BoxCox", "center", "scale"))
# and now we create the transformed Glass
GlassAfterAllTrans <- predict(allTrans, Glass[-length(Glass)])

# Can we use all predictors or are there any with near zero variance
nearZeroVar(Glass[-length(Glass)])
# Should we eliminate any presictors because of between-predictor correlation?
# Let's calculate the correlations and then plot them
correlations <- cor(Glass[-length(Glass)])
library(corrplot)
corrplot(correlations, order="hclust")
# We can also use the findCorrelation() function which recommend which
# predictors are recommended for removal
highCorr <- findCorrelation(correlations, cutoff = .75)
# since highCorr is not empty, we apply it to remove the correlated material
filteredGlass <- Glass[, -highCorr]
