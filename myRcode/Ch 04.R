## Chapter 4 Computing and Exercises
# Set up the environment
library(AppliedPredictiveModeling)
library(caret)
library(e1071)
#library(Design)
library(ipred)
library(MASS)

# load sample data
data(twoClassData)

# Split sample data
set.seed(1)         # so we can reproduce the results
trainingRows <- createDataPartition(classes,            # data set to partition
                                    p=0.80,             # percent of stratified samples
                                    list=FALSE )        # return the result as a matrix
                                                        # instead of a list (default)
# trainingRows contains the indices onto the dataset to extract
# now we can subset the data
# training data set
trainPredictors <- predictors[trainingRows, ]
trainClasses <- classes[trainingRows]
# test data set
testPredictors <- predictors[-trainingRows, ]
testClasses <- classes[-trainingRows]
