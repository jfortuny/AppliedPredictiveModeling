## Exercises Chapter 3

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
plotOne <- function(d, n, i) {
  v <- d[i]
  h <- n[i]
  hist(v)
  title(h)
}