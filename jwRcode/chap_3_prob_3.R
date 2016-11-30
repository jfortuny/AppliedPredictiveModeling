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

# Exercise 3 (EPage 54): 
#
library(caret)
data(BloodBrain)

# Look for degenerate columns: 
zero_cols = nearZeroVar( bbbDescr )
colnames( bbbDescr )[ zero_cols ]

# Look for strong correlations among the predictors: 
library(corrplot)
corrplot( cor( bbbDescr ), order="hclust" )

# Find which predictors we can elliminate since they have correlations that are "too large":
#
highCorr = findCorrelation( cor( bbbDescr ), cutoff=0.75 )

bbbDescr_independent = bbbDescr[,-highCorr]

corrplot( cor(bbbDescr_independent) ) # notice that this matrix has no values > cutoff=0.75 above


