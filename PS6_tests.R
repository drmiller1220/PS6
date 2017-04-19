##################################

setwd("C://Users//drmiller1220//Documents//GitHub//PS6")

library(testthat)

source("PS6.R")

### Checking that built-in errors works

test_that("Number of bounds must be equal", {
  expect_error(sg.mult.par(test_func, lower=c(0,1), upper=c(2,5,4)))
})

test_that("Each lower must be smaller than each upper", {
  expect_error(sg.mult.par(test_func, lower=c(0,1), upper=c(2,0)))
})

## testing non-parallelized and parallelized functions for 2-dimensions

function_2d <- function(x){
  x[1]^2 + 2*x[2]
}

lower_2d <- c(0,1)
upper_2d <- c(2,4)

# integral of the function is 38;
# over 0_2 and 1_4, x_1^2+2*x_2
# x_1^3/3 + 2x_1+x_2
# 8/3 + 4x_2-0
# 8x_2/3 + 4x_2^2/2
# 8x_2/3 + 2_2^2
# 8*4/3 + 2(4^2) -8/3 -2
#32/3 - 8/3 + 32 -2
# 8+32 -2=38


expect_equal(as.numeric(sg.mult.par(function_2d, lower=lower_2d, upper=upper_2d)),
             38, tolerance=15)

expect_equal(as.numeric(sg.mult(function_2d, lower=lower_2d, upper=upper_2d)),
             38, tolerance=15)

## testing non-parallelized and parallelized functions for 3-dimensions

function_3d <- function(x){
  x[1]^2 + 2*x[2] + x[3]
}

lower_3d <- c(0,-1,-2)
upper_3d <- c(3,2,2)

# integral of the function is 144;
# over 0_3 and -1_2 and -2_2, x^2 + 2y +z
# x^3/3 + 2xy + xz
# 9 + 6y + 3z -0
# 9y + 6y^2/2 + 3yz
# 18 + 24/2 + 6z + 9 - 3 + 3z
# 18 + 12 + 6z + 6 + 3z
# 36 + 9z
# 36z + 9z^2
# 36*2+ 36/2 + 36*2 - 36/2
# 144


expect_equal(as.numeric(sg.mult.par(function_3d, lower=lower_3d, upper=upper_3d)),
             144, tolerance=15)

expect_equal(as.numeric(sg.mult(function_3d, lower=lower_3d, upper=upper_3d)),
             144, tolerance=15)