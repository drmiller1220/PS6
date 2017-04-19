##################################

## Testing speed when running the function in parallel; sg.mult and sg.mult.par are the
## same functions, except that sg.mult.par runs the function in parallel, so we want to compare
## the performance of sg.mult and sg.mult.par

setwd("C://Users//drmiller1220//Documents//GitHub//PS6")

source("PS6.R") # reading in file with functions

library(microbenchmark)
library(cubature)

# measuring speed for 2 dimensions

function_2d <- function(x){
  x[1]^2 + 2*x[2]
}

lower_2d <- c(0,1)
upper_2d <- c(2,4)

microbenchmark(sg.mult(function_2d, lower=lower_2d, upper=upper_2d),
               sg.mult.par(function_2d, lower=lower_2d, upper=upper_2d), times = 50)

# non-parallelized version is substantially faster

############################################

# measuring speed for 3 dimensions

function_3d <- function(x){
  2*x[1] + x[2]^2 + 1.2*x[3]
}

lower_3d <- c(0,3,5)
upper_3d <- c(4,9,11)

microbenchmark(sg.mult(function_3d, lower=lower_3d, upper=upper_3d),
               sg.mult.par(function_3d, lower=lower_3d, upper=upper_3d), times = 50)

# non-parallelized version is still substantially faster

############################################

# measuring speed for 4 dimensions

function_4d <- function(x){
  2*x[1] + x[2]^2 + 1.2*x[3] + x[4]^0.5
}

lower_4d <- c(0,3,5,2)
upper_4d <- c(4,9,11,6)

microbenchmark(sg.mult(function_4d, lower=lower_4d, upper=upper_4d),
               sg.mult.par(function_4d, lower=lower_4d, upper=upper_4d), times=10)

# non-parallelized version is still, but the gap is smaller faster

###########################################
# measuring speed for 5 dimensions

function_5d <- function(x){
  2*x[1] + x[2]^2 + 1.2*x[3] + x[4]^0.5 + 5*x[5]
}

lower_5d <- c(0,3,5,2,1)
upper_5d <- c(4,9,11,6,12)

microbenchmark(sg.mult(function_5d, lower=lower_5d, upper=upper_5d),
               sg.mult.par(function_5d, lower=lower_5d, upper=upper_5d), times=10)

# non-parallelized version is a little faster, but the gap is fairly small

##################

### for 2 to 5 dimensions, adaptIntegrate is much faster than the sparse grid approach

microbenchmark(adaptIntegrate(function_2d, lowerLimit = lower_2d, upperLimit = upper_2d),
               sg.mult.par(function_2d, lower=lower_2d, upper=upper_2d), times=50)

microbenchmark(adaptIntegrate(function_3d, lowerLimit = lower_3d, upperLimit = upper_3d),
               sg.mult.par(function_3d, lower=lower_3d, upper=upper_3d), times=50)

microbenchmark(adaptIntegrate(function_4d, lowerLimit = lower_4d, upperLimit = upper_4d),
               sg.mult.par(function_4d, lower=lower_4d, upper=upper_4d), times=10)

microbenchmark(adaptIntegrate(function_5d, lowerLimit = lower_5d, upperLimit = upper_5d),
               sg.mult.par(function_5d, lower=lower_5d, upper=upper_5d), times=10)

###################################################################

### adaptIntegrate is also much more accurate than the sparse grid approach; below, we lot
### at how accurate both functions are given the 2D and 3D examples provided above for
### our tests

function_2d_test <- function(x){
  x[1]^2 + 2*x[2]
}

lower_2d_test <- c(0,1)
upper_2d_test <- c(2,4)

# computational answer is 38; adaptIntegrate returns 38, sparse grid returns
# approximately 46
adaptIntegrate(function_2d_test, lowerLimit = lower_2d_test, upperLimit = upper_2d_test)
sg.mult.par(function_2d_test, lower=lower_2d_test, upper=upper_2d_test)

############################################

function_3d_test <- function(x){
  x[1]^2 + 2*x[2] + x[3]
}

lower_3d_test <- c(0,-1,-2)
upper_3d_test <- c(3,2,2)

# computational answer is 144; adaptIntegrate returns 144, sparse grid returns
# approximately 138
adaptIntegrate(function_3d_test, lowerLimit = lower_3d_test, upperLimit = upper_3d_test)
sg.mult.par(function_3d_test, lower=lower_3d_test, upper=upper_3d_test)