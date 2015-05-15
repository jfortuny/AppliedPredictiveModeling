# Chapter 6 Computing and Exercises
# 6.5 Computing

# Set things up
library(AppliedPredictiveModeling)
data(solubility)
library(lattice)

## The data objects begin with "sol"
ls(pattern="^solT")

### Some initial plots of the data
xyplot(solTrainY ~ solTrainX$MolWeight, type = c("p", "g"),
       ylab = "Solubility (log)",
       main = "(a)",
       xlab = "Molecular Weight")
xyplot(solTrainY ~ solTrainX$NumRotBonds, type = c("p", "g"),
       ylab = "Solubility (log)",
       xlab = "Number of Rotatable Bonds")
bwplot(solTrainY ~ ifelse(solTrainX[,100] == 1, 
                          "structure present", 
                          "structure absent"),
       ylab = "Solubility (log)",
       main = "(b)",
       horizontal = FALSE)

### Find the columns that are not fingerprints (i.e. the continuous
### predictors). grep will return a list of integers corresponding to
### column names that contain the pattern "FP".
notFingerprints <- grep("FP", names(solTrainXtrans))

library(caret)
featurePlot(solTrainXtrans[, -notFingerprints],
            solTrainY,
            between = list(x = 1, y = 1),
            type = c("g", "p", "smooth"),
            labels = rep("", 2))

### We used the full namespace to call this function because the pls
### package (also used in this chapter) has a function with the same
### name.
library(corrplot)
corrplot::corrplot(cor(solTrainXtrans[, -notFingerprints]), 
                   order = "hclust", 
                   tl.cex = .8)

### 6.5.1 Ordinary Linear Regression
# Since lm requires a single data frame for the data, create it by combining predictors and outcome
trainingData <- solTrainXtrans
trainingData$solubility <- solTrainY
# now fit the lm model
lmFitAllPredictors <- lm(solubility ~ ., data=trainingData)
summary(lmFitAllPredictors)
# to predict new samples use predict()
lmPred1 <- predict(lmFitAllPredictors, solTestXtrans)
head(lmPred1)
# Collect observed and predicted values for the test data set into a dataframe
# to evaluate the performance of the test using defaultSummary()
lmValues1 <- data.frame(obs=solTestY, pred=lmPred1)
library(caret)
defaultSummary(lmValues1)
library(lattice)
xyplot(lmValues1$pred ~ lmValues1$obs, type = c("p", "g"),
       ylab = "Predicted",
       main = "PRedicted vs. Observed",
       xlab = "Observed")

# To use resampling:
### Create a control function that will be used across models. We
### create the fold assignments explicitly instead of relying on the
### random number seed being set to identical values.

set.seed(100)
indx <- createFolds(solTrainY, k = 10, list = TRUE, returnTrain = TRUE)
ctrl <- trainControl(method = "cv", index = indx)

### Linear regression model with all of the predictors. This will
### produce some warnings that a 'rank-deficient fit may be
### misleading'. This is related to the predictors being so highly
### correlated that some of the math has broken down.

set.seed(100)
lmTune0 <- train(x = solTrainXtrans, y = solTrainY,
                 method = "lm",
                 trControl = ctrl)
# This model includes all the predictors
lmTune0     
# Validate:
# Frst plot the observed values vs. the predicted values (are we tracking?); then
# plot the residuals vs. the predicted values (are we missing any terms in the model?)
xyplot(solTrainY ~ predict(lmTune0),
       type = c("p", "g"),      # plot points (p) and a background grid (g)
       xlab = "Predicted", ylab = "Observed", main = 'Observed vs. Predicted')
xyplot(resid(lmTune0) ~ predict(lmTune0),
       type = c("p", "g"),
       xlab = "Predicted", ylab = "Residuals", main = "Residuals vs. Predicted")

### Create another model using a set of predictors reduced by unsupervised
### filtering. We apply a filter to reduce extreme between-predictor
### correlations. Note the lack of warnings.

tooHigh <- findCorrelation(cor(solTrainXtrans), .9)
trainXfiltered <- solTrainXtrans[, -tooHigh]
testXfiltered  <-  solTestXtrans[, -tooHigh]

set.seed(100)
lmTune <- train(x = trainXfiltered, y = solTrainY,
                method = "lm",
                trControl = ctrl)

lmTune

### Save the test set results in a data frame                 
testResults <- data.frame(obs = solTestY,
                          Linear_Regression = predict(lmTune, testXfiltered))

# Section 6.5.2 Partial Least Squares
library(pls)
# Invoke plsr() without limiting the number of components (parameter ncomp)
plsFit <- plsr(solubility ~ ., data=trainingData)
# summary(plsFit)
# plsFit
# To predict use predict()
predict(plsFit, solTestXtrans[1:5,], ncomp=1:2)

## Run PLS and PCR on solubility data and compare results
set.seed(100)
plsTune <- train(x = solTrainXtrans, y = solTrainY,
                 method = "pls",
                 tuneGrid = expand.grid(ncomp = 1:20),
                 trControl = ctrl)
plsTune

testResults$PLS <- predict(plsTune, solTestXtrans)

set.seed(100)
pcrTune <- train(x = solTrainXtrans, y = solTrainY,
                 method = "pcr",
                 tuneGrid = expand.grid(ncomp = 1:35),
                 trControl = ctrl)
pcrTune                  

plsResamples <- plsTune$results
plsResamples$Model <- "PLS"
pcrResamples <- pcrTune$results
pcrResamples$Model <- "PCR"
plsPlotData <- rbind(plsResamples, pcrResamples)

xyplot(RMSE ~ ncomp,
       data = plsPlotData,
       #aspect = 1,
       xlab = "# Components",
       ylab = "RMSE (Cross-Validation)",
       auto.key = list(columns = 2),
       groups = Model,
       type = c("o", "g"))

plsImp <- varImp(plsTune, scale = FALSE)
plot(plsImp, top = 25, scales = list(y = list(cex = .95)))

### Section 6.5.3 Penalized Models
library(elasticnet)
ridgeModel <- enet(x = as.matrix(solTrainXtrans), 
                   y = solTrainY,
                   lambda = 0.001)    # lambda specifies the ridge-regression penalty

## The text used the elasticnet to obtain a ridge regression model.
## There is now a simple ridge regression method.

ridgeGrid <- expand.grid(lambda = seq(0, .1, length = 15))

set.seed(100)
ridgeTune <- train(x = solTrainXtrans, y = solTrainY,
                   method = "ridge",
                   tuneGrid = ridgeGrid,
                   trControl = ctrl,
                   preProc = c("center", "scale"))
ridgeTune

print(update(plot(ridgeTune), xlab = "Penalty"))


enetGrid <- expand.grid(lambda = c(0, 0.01, .1), 
                        fraction = seq(.05, 1, length = 20))
set.seed(100)
enetTune <- train(x = solTrainXtrans, y = solTrainY,
                  method = "enet",
                  tuneGrid = enetGrid,
                  trControl = ctrl,
                  preProc = c("center", "scale"))
enetTune

plot(enetTune)

testResults$Enet <- predict(enetTune, solTestXtrans)
