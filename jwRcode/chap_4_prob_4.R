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

library(caret)

data(oil)

set.seed(0)

str(oilType)
table(oilType)

# What is the population frequency of oil types:
#
print( table(oilType)/length(oilType) )

# Draw a sample:
# 
a_sample = sample( oilType, 60, replace=TRUE )

# How do its frequencies compare with that of the population:
# 
print( table(a_sample)/length(a_sample) )

another_sample = createDataPartition( oilType, p=0.625 )

print( table(oilType[ another_sample$Resample1 ])/length(another_sample$Resample1) )

num_correct = 16:20
width_of_interval = c()
for( nc in num_correct ){
  bt_out = binom.test( nc, 20 )
  width_of_interval = c( width_of_interval, diff( bt_out$conf.int ) )
}
if( save_plots ){ postscript("../../WriteUp/Graphics/Chapter4/chap_4_prob_4_ci_width.eps", onefile=FALSE, horizontal=FALSE) }
plot( num_correct, width_of_interval, type='l', xlab='number of correct samples (from 20)', ylab='width of 95% confidenci interval' )
grid() 
if( save_plots ){ dev.off() }
















































































if( save_plots ){ postscript("../../WriteUp/Graphics/Chapter4/chap_4_prob_4.eps", onefile=FALSE, horizontal=FALSE) }
if( save_plots ){ dev.off() }


