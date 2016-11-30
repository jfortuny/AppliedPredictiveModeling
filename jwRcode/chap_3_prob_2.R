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

# Exercise 2 (EPage 54): 
#
library(mlbench)
data(Soybean)

library( caret ) 
zero_cols = nearZeroVar( Soybean )
colnames( Soybean )[ zero_cols ]
Soybean = Soybean[,-zero_cols] 

# Count how many NA's we have in each feature:
#
apply( Soybean, 2, function(x){ sum(is.na(x)) } )

# See if a class has more NA's than others:
#
Soybean$has_nans_in_sample = apply( Soybean[,-1], 1, function(x){ sum(is.na(x)) > 0 } )
table( Soybean[, c(1,34) ] )

# For imputation of data for the NA's
# 
#library( caret )
#preProcess( Soybean[,-1], method=c("knnImpute"), na.remove=FALSE ) 
