# 2 Visualizations
str(iris)

library(AppliedPredictiveModeling)
transparentTheme(trans = 0.4)

library(caret)
featurePlot(x = iris[,1:4],
            y = iris$Species,
            plot = "pairs",
            auto.key = list(columns=3))
featurePlot(x = iris[,1:4],
            y = iris$Species,
            plot = "ellipse",
            auto.key = list(columns=3))

transparentTheme(0.9)
featurePlot(x = iris[,1:4],
            y = iris$Species,
            plot = "density",
            scales = list(x = list(relation="free"),
                          y = list(relation="free")),
            adjust = 1.5,
            pch = "|",
            layout = c(4,1),
            auto.key = list(columns=3))

featurePlot(x = iris[,1:4],
            y = iris$Species,
            plot = "box",
            scales = list(x = list(rot = 90),
                          y = list(relation="free")),
            layout = c(4,1),
            auto.key = list(columns=2))

# 3.1 Creating Dummy variables
library(earth)
data("etitanic")
str(etitanic)

# standard R way
head(etitanic)
head(model.matrix(survived ~ ., data = etitanic))

# using caret::dummyvars
dummies <- dummyVars(survived ~ ., data = etitanic)
head(predict(dummies, newdata = etitanic))


# 3.2 Zero- and Near Zero-Variance predictors
data(mdrr)
head(mdrrDescr)
data.frame(table(mdrrDescr$nR11))

nzv <- nearZeroVar(mdrrDescr, saveMetrics = TRUE)
head(nzv)
head(nzv[nzv$nzv,], 10)

# without the metrics
nzv <- nearZeroVar(mdrrDescr)
filteredDescr <- mdrrDescr[, -nzv]
dim(mdrrDescr)
dim(filteredDescr)
names(filteredDescr)

# 3.3 Correlated Predictors
descrCor <- cor(filteredDescr)
highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > 0.999)
# So 65 descriptors are highly correlated
summary(descrCor[upper.tri(descrCor)])

# using findCorrelation
highlyCorDescr <- findCorrelation(descrCor, cutoff = 0.75)
filteredDescr <- filteredDescr[, - highlyCorDescr]
descrCor2 <- cor(filteredDescr)
summary(descrCor2[upper.tri(descrCor2)])

# 3.6 Centering and Scaling
set.seed(96)
inTrain <- sample(seq(along = mdrrClass), length(mdrrClass)/2)
training <- filteredDescr[inTrain,]
test <- filteredDescr[-inTrain,]
trainMDRR <- mdrrClass[inTrain]
testMDRR <- mdrrClass[-inTrain]

preProcValues <- preProcess(training, method = c("center", "scale"))
trainTransformed <- predict(preProcValues, training)
testTransformed <- predict(preProcValues, test)
head(training)
head(trainTransformed)

# 3.9 Putting it all together
data("schedulingData")
str(schedulingData)

# define preProcess parameters
pp_hpc <- preProcess(schedulingData[,-8],
                     method = c("center", "scale", "YeoJohnson"))
pp_hpc
# apply preProcess parameters to data
transformed <- predict(pp_hpc, newdata = schedulingData[, -8])
head(schedulingData)
head(transformed)
summary(transformed)
hist(transformed$Hour)
sum(schedulingData$NumPending == 0)/nrow(schedulingData)
mean(schedulingData$NumPending == 0)

pp_no_nzv <- preProcess(schedulingData[, -8],
                        method = c("center", "scale", "YeoJohnson", "nzv"))
pp_no_nzv

predict(pp_no_nzv, newdata = schedulingData[1:6, -8])

# 3.10 Class distance calculations
data(mdrr)
head(mdrrClass)
head(mdrrDescr)

# incomplete because trainBC is not available
centroids <- classDist(trainBC, trainMDRR)

# 4.1 Data Splitting based on the outcome
set.seed(3456)
trainIndex <- createDataPartition(iris$Species, p = 0.8, list = FALSE, times = 1)
head(trainIndex)

irisTrain <- iris[trainIndex,]
irisTest <- iris[-trainIndex,]
