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

library(caret) 

# Exercise 1
#
set.seed(0)

# Generate some class labels (of nonuniform classes): 
#
n = 1000
classes = sample( c(1,2,3), n, replace=TRUE, prob=c(0.7,0.2,0.1) )  

# Show that our class data gives the correct proporations:
#
print( table( classes )/n )

# Demonstrate the use of createDataPartition:
#
split = createDataPartition( classes, p=0.8 )

# Verify that we have a stratified sample that "matches" our prior distribution:
# 
T = table( classes[ split$Resample1 ] )
print( T/sum(T) )
