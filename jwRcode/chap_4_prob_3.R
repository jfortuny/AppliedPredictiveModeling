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

library(Hmisc)

library(AppliedPredictiveModeling)
data(ChemicalManufacturingProcess)

set.seed(0)

# Get the given data into a form we can plot:
#
components = 1:10
means = c( 0.444, 0.500, 0.533, 0.545, 0.542, 0.537, 0.534, 0.534, 0.520, 0.507 )
std_errors = c( 0.0272, 0.0298, 0.0302, 0.0308, 0.0322, 0.0327, 0.0333, 0.0330, 0.0326, 0.0324 )
data = data.frame( components, means, std_errors ) 

if( save_plots ){ postscript("../../WriteUp/Graphics/Chapter4/chap_4_prob_3_R2_plot.eps", onefile=FALSE, horizontal=FALSE) }
errbar( components, means, means+std_errors, means-std_errors )  
grid()
max_index = which.max( means )
abline( h=means[max_index] - std_errors[max_index], col='red' )
if( save_plots ){ dev.off() }


