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
