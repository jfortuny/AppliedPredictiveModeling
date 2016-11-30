#
# Written by:
# -- 
# John L. Weatherwax                2009-04-21
# 
# email: wax@alum.mit.edu
# 
# Please send comments and especially bug reports to the
# above email address.
# 
#-----

save_plots = F

# Exercise 1 (EPage 52): 
#
library(mlbench)
data(Glass)
str(Glass)

# Look at all pairwise scatter plots:
# 
pairs(Glass)

# Look at all the correlation between all predictors:
# 
cor( Glass[,-10] ) # drop the factor variable (the last 10th one)

# Look at the correlation of each predictor with the class label:
#
cor( Glass[,-10], as.numeric( Glass[,10] ) )

# Visually display how Mg and Al depend on the glass type:
par(mfrow=c(1,2)) 
boxplot( Glass$Mg ~ Glass$Type )
boxplot( Glass$Al ~ Glass$Type )
par(mfrow=c(1,1))

# Use the "corrplot" command:
#
library(corrplot)
corrplot( cor( Glass[,-10] ), order="hclust" )

# Look for the features with the most number of outliers:
# 
if( save_plots ){ postscript("../../WriteUp/Graphics/Chapter3/chap_3_prob_1_boxplot.eps", onefile=FALSE, horizontal=FALSE) }
boxplot( Glass ) 
if( save_plots ){ dev.off() } 

# Compute the skewness of each feature:
library(e1071)
apply( Glass[,-10], 2, skewness )

# Look at histograms of some of the skewed predictors:
if( save_plots ){ postscript("../../WriteUp/Graphics/Chapter3/chap_3_prob_1_skewed_predictors.eps", onefile=FALSE, horizontal=FALSE) }
par(mfrow=c(1,3))
hist( Glass$K ) # Looks like a data error in that we have only two  samples with a very large K value 
hist( Glass$Ba ) # Looks like a skewed distribution
hist( Glass$Mg ) # Looks multimodal
par(mfrow=c(1,1))
if( save_plots ){ dev.off() } 

# Transform our predictors using the Box-Cox tranformation:
#
library(caret) # to get BoxCoxTrans
Glass$Mg = Glass$Mg + 1.e-6 # add a small value so that BoxCoxTransfs will converge 
Glass$K = Glass$K + 1.e-6
Glass$Ba = Glass$Ba + 1.e-6
Glass$Fe = Glass$Fe + 1.e-6

boxcox_skewness = function(x){
  BCT = BoxCoxTrans(x)
  x_bc = predict( BCT, x )
  skewness(x_bc) 
}

apply( Glass[,-10], 2, boxcox_skewness )

