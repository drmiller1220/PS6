given_func <- function(x){                     # coding original function given
  sin(x[1]^2/2-x[2]^2/4)*cos(2*x[1]-exp(x[2]))
}

# using optim with many different starting values

optim(par=c(-.5,-.5), fn=given_func, lower=c(-1,-1), upper=c(3,3), method="L-BFGS-B",
      control=list(fnscale=-1))

optim(par=c(0,0), fn=given_func, lower=c(-1,-1), upper=c(3,3), method="L-BFGS-B",
      control=list(fnscale=-1))

optim(par=c(.5,.5), fn=given_func, lower=c(-1,-1), upper=c(3,3), method="L-BFGS-B",
      control=list(fnscale=-1))

optim(par=c(1,1), fn=given_func, lower=c(-1,-1), upper=c(3,3), method="L-BFGS-B",
      control=list(fnscale=-1))

optim(par=c(1.5,1.5), fn=given_func, lower=c(-1,-1), upper=c(3,3), method="L-BFGS-B",
      control=list(fnscale=-1))

optim(par=c(2,2), fn=given_func, lower=c(-1,-1), upper=c(3,3), method="L-BFGS-B",
      control=list(fnscale=-1))

optim(par=c(2.5,2.5), fn=given_func, lower=c(-1,-1), upper=c(3,3), method="L-BFGS-B",
      control=list(fnscale=-1))

# using optimize with many different starting values

given_func_2args <- function(x,y){      # coding original function given
  sin(x^2/2-y^2/4)*cos(2*x-exp(y))      # to take two arguments
}

# breaking up the range -1 to 3, we optimize for each segment and are returned the maximum
# for each segment
segments <- lapply(seq(-1,3,0.000001), function(z) optimize(given_func_2args, y=z,
                                                            lower = c(-1,-1),
                                                            upper = c(3,3),
                                                            maximum = TRUE))

# given a list of the maxima for each segment, we identify the index of the maximum value
# which is the maximum of the maxima

local_max <- which(unlist(lapply(segments, function(x) x$maximum)) == 
                        max(unlist(lapply(segments, function(x) x$maximum))))

# prints the x and y values of the local maximum, as well as the maximum value
show_max <- c(x=segments[local_max][[1]]$maximum, y=seq(-1,3,0.000001)[local_max],
              lmax=segments[local_max][[1]]$objective)
show_max

# the maximum value identified is 0.997, with x=1.99 and y =1.37, which is approximately
# equal to the values for x and y of 2.031 1.402 for the moderate values of optim
