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

# 4.9.1 - Split sample data
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

# 4.9.2 - Resampling
set.seed(1)
# caret has these functions for resampling:
#     - createDataPartition()    - create stratified random splits of a dataset
#     - createResamples()        - create bootstrap samples (simple random sampling is used)
#     - createFolds()            - splits the data into k folds
#     - createMultiFolds()       - creates multiple samples of independent k-folds
# For random splits:
repeatedSplits <- createDataPartition(trainClasses,   # data set to partition
                                      p=0.8,          # percent of members in sample
                                      times=3)        # number of resamples
# or for k-fold splits:
cvSplits <- createFolds(trainClasses,        # data set to partition
                        k=10,                # number of folds
                        returnTrain=TRUE)    # 
# The first fold is the first list in cvSplits and it only contains the training set (note the counts)
fold1 <- cvSplits[[1]]
cvPredictors1 <- trainPredictors[fold1,]
cvClasses1 <- trainClasses[fold1,]
nrow(trainPredictors)
nrow(cvPredictors1)

# 4.9.4 - Tuning Parameters' selection
data(GermanCredit)
## First, remove near-zero variance predictors then get rid of a few predictors 
## that duplicate values. For example, there are two possible values for the 
## housing variable: "Rent", "Own" and "ForFree". So that we don't have linear
## dependencies, we get rid of one of the levels (e.g. "ForFree")

GermanCredit <- GermanCredit[, -nearZeroVar(GermanCredit)]
GermanCredit$CheckingAccountStatus.lt.0 <- NULL
GermanCredit$SavingsAccountBonds.lt.100 <- NULL
GermanCredit$EmploymentDuration.lt.1 <- NULL
GermanCredit$EmploymentDuration.Unemployed <- NULL
GermanCredit$Personal.Male.Married.Widowed <- NULL
GermanCredit$Property.Unknown <- NULL
GermanCredit$Housing.ForFree <- NULL

## Split the data into training (80%) and test sets (20%)
set.seed(100)
inTrain <- createDataPartition(GermanCredit$Class, p = .8)[[1]]
GermanCreditTrain <- GermanCredit[ inTrain, ]
GermanCreditTest  <- GermanCredit[-inTrain, ]

## The model fitting code shown in the computing section is fairly
## simplistic.  For the text we estimate the tuning parameter grid
## up-front and pass it in explicitly. This generally is not needed,
## but was used here so that we could trim the cost values to a
## presentable range and to re-use later with different resampling
## methods.

library(kernlab)
set.seed(231)
sigDist <- sigest(Class ~ ., data = GermanCreditTrain, frac = 1)
svmTuneGrid <- data.frame(sigma = as.vector(sigDist)[1], C = 2^(-2:7))

set.seed(1056)
svmFit <- train(Class ~ .,
                data = GermanCreditTrain,
                method = "svmRadial",
                preProc = c("center", "scale"),
                tuneGrid = svmTuneGrid,
                trControl = trainControl(method = "repeatedcv", 
                                         repeats = 5,
                                         classProbs = TRUE))
## classProbs = TRUE was added since the text was written

## Print the results
svmFit

## A line plot of the average performance. The 'scales' argument is actually an 
## argument to xyplot that converts the x-axis to log-2 units.

plot(svmFit, scales = list(x = list(log = 2)))

## Test set predictions

predictedClasses <- predict(svmFit, GermanCreditTest)
str(predictedClasses)

## Use the "type" option to get class probabilities

predictedProbs <- predict(svmFit, newdata = GermanCreditTest, type = "prob")
head(predictedProbs)


## Fit the same model using different resampling methods. The main syntax change
## is the control object.

set.seed(1056)
svmFit10CV <- train(Class ~ .,
                    data = GermanCreditTrain,
                    method = "svmRadial",
                    preProc = c("center", "scale"),
                    tuneGrid = svmTuneGrid,
                    trControl = trainControl(method = "cv", number = 10))
svmFit10CV
plot(svmFit10CV, scales = list(x = list(log = 2)))

set.seed(1056)
svmFitLOO <- train(Class ~ .,
                   data = GermanCreditTrain,
                   method = "svmRadial",
                   preProc = c("center", "scale"),
                   tuneGrid = svmTuneGrid,
                   trControl = trainControl(method = "LOOCV"))
svmFitLOO
plot(svmFitLOO, scales = list(x = list(log = 2)))

set.seed(1056)
svmFitLGO <- train(Class ~ .,
                   data = GermanCreditTrain,
                   method = "svmRadial",
                   preProc = c("center", "scale"),
                   tuneGrid = svmTuneGrid,
                   trControl = trainControl(method = "LGOCV", 
                                            number = 50, 
                                            p = .8))
svmFitLGO 

set.seed(1056)
svmFitBoot <- train(Class ~ .,
                    data = GermanCreditTrain,
                    method = "svmRadial",
                    preProc = c("center", "scale"),
                    tuneGrid = svmTuneGrid,
                    trControl = trainControl(method = "boot", number = 50))
svmFitBoot

set.seed(1056)
svmFitBoot632 <- train(Class ~ .,
                       data = GermanCreditTrain,
                       method = "svmRadial",
                       preProc = c("center", "scale"),
                       tuneGrid = svmTuneGrid,
                       trControl = trainControl(method = "boot632", 
                                                number = 50))
svmFitBoot632

# Section 4.9.5 Choosing Between Models

# Compare svmFit against logisticReg (logistic Regression); 
# first let's fit with Logistic Regression
set.seed(1056)
logisticReg <- train(Class ~ .,
                    data = GermanCreditTrain,
                    method = "glm",
                    trControl = trainControl(method = "repeatedcv", 
                                             repeats = 5))
logisticReg


# Now compare
resamp <- resamples(list(SVM = svmFit, Logistic = logisticReg))
summary(resamp)

## These results are slightly different from those shown in the text.
## There are some differences in the train() function since the 
## original results were produced. This is due to a difference in
## predictions from the ksvm() function when class probs are requested
## and when they are not. See, for example, 
## https://stat.ethz.ch/pipermail/r-help/2013-November/363188.html

modelDifferences <- diff(resamp)
summary(modelDifferences)

## The actual paired t-test:
modelDifferences$statistics$Accuracy
