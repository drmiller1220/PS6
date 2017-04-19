## commenting original function

sg.int<-function(g,...,lower,upper){ # initializing function; takes inputs of a function
                                     # over which to integrate, as well as lower and upper
                                     # bounds
  require("SparseGrid") # loading in required library
  require("parallel") # 
  
  lower<-floor(lower) # takes in lower bounds, sets to next-nearest smallest integers
  
  upper<-ceiling(upper) # takes in upper bounds, sets to next nearest larger integers
  
  if (any(lower>upper)) stop("lower must be smaller than upper")
                        # checks to make sure that all upper bounds are larger than the 
                        # corresponding lower bounds
  
  if (length(lower)!=length(upper)) stop("number of lower and upper bounds must match")
  # checks to make sure that the user inputs the same number of lower and upper bounds
  
  gridss<-as.matrix(expand.grid(seq(lower[1],upper[1]-1,by=1),
                                seq(lower[2],upper[2]-1,by=1)))
                        # create a matrix with the unique combinations of integers from the
                        # lower to upper bounds of each unique set
  
  sp.grid <- createIntegrationGrid( 'KPU', dimension=2, k=5 )
                        # create nodes and weights that are used for sparse grid integration;
                        # first argument is the type of one-dimensional integration rule;
                        # second argument indicates the dimensionality of the integration
                        # problem; third argument indicates the accuracy level
  
  nodes<-gridss[1,]+sp.grid$nodes
            # initializes our nodes with the first row of gridss plus our nodes from
            # createIntegrationGrid
  
  weights<-sp.grid$weights
            # initializes our weights with the weights from createIntegrationGrid
  
  for (i in 2:nrow(gridss))
    
  {
    nodes<-rbind(nodes,gridss[i,]+sp.grid$nodes)  
    
    weights<-c(weights,sp.grid$weights)
    
  }
  
  # for loop takes each row in gridss after the first row, and adds the node values to
  # each of these rows; also adds the matrix of weights to weights for each row
  
  gx.sp <- apply(nodes, 1, g,...) # applies the function g to the nodes
  val.sp <- gx.sp %*%weights # multiplies gx.sp by the weights
  val.sp # returns val.sp
}

## testing original function with a toy function

test_func <- function(x){
  x[1]^2+x[2]*3
}

lower <- c(0,1)
upper <- c(3,7)

sg.int(test_func, lower=lower, upper=upper)

#############################

# now making the function multidimensional and ``apply"-ing where possible

sg.mult<-function(g,...,lower,upper){ # initializing function; takes inputs of a function
  # over which to integrate, as well as lower and upper
  # bounds
  require("SparseGrid") # loading in required library
  require("parallel") # 
  
  lower<-floor(lower) # takes in lower bounds, sets to next-nearest smallest integers
  
  upper<-ceiling(upper) # takes in upper bounds, sets to next nearest larger integers
  
  if (any(lower>upper)) stop("lower must be smaller than upper")
  # checks to make sure that all upper bounds are larger than the 
  # corresponding lower bounds
  
  if (length(lower)!=length(upper)) stop("number of lower and upper bounds must match")
  # checks to make sure that the user inputs the same number of lower and upper bounds
  
  sequences <- list()
  sequences <- lapply(1:length(lower), function(x) seq(lower[x],upper[x]-1,by=1))
  gridss <- as.matrix(expand.grid(sequences))
  # create a matrix with the unique combinations of integers from the
  # lower to upper bounds of each unique set
  # in order to do so for higher dimensions, we need to store the sequences for each pair of
  # lower and upper bounds in a list, and then use lapply to extract these sequences.  Then,
  # given the sequences, we create the matrix with expand.grid
  # this replaces the for loop in the first iteration
  
  sp.grid <- createIntegrationGrid( 'KPU', dimension=length(lower), k=5 )
  # create nodes and weights that are used for sparse grid integration;
  # first argument is the type of one-dimensional integration rule;
  # second argument indicates the dimensionality of the integration
  # problem; third argument indicates the accuracy level
  
  nodes <- lapply(1:nrow(gridss), function(x) gridss[x,]+sp.grid$nodes)
  nodes <- do.call(rbind, nodes)
  # initializes our nodes with the first row of gridss plus our nodes from
  # createIntegrationGrid; replaces part of for loop by creating a list of each set of grids,
  # and then rbinding each matrix together
  
  weights <- rep(sp.grid$weights, nrow(gridss))
  # replaces the for loop for weights by simply repeating the weights for the number of
  # rows in gridss
  
  gx.sp <- apply(nodes, 1, g,...) # applies the function g to the nodes
  val.sp <- gx.sp %*%weights # multiplies gx.sp by the weights
  val.sp # returns val.sp
}

test_func <- function(x){
  x[1]^2+x[2]*3+x[3]^0.5
}

lower <- c(0,1,4)
upper <- c(1,10,8)

sg.mult(test_func, lower=lower, upper=upper)

#############################

# now parallelizing the function

sg.mult.par<-function(g,...,lower,upper){ # initializing function; takes inputs of a function
  # over which to integrate, as well as lower and upper
  # bounds
  require("SparseGrid") # loading in required library
  require("parallel") # 
  
  num_cores <- detectCores() -1 # detecting number of cores minus 1 to allow user to
                                # use their computer for other purposes while running
  cl <- makeCluster(num_cores) # making cluster
  
  lower<-floor(lower) # takes in lower bounds, sets to next-nearest smallest integers
  
  upper<-ceiling(upper) # takes in upper bounds, sets to next nearest larger integers
  
  if (any(lower>upper)) stop("lower must be smaller than upper")
  # checks to make sure that all upper bounds are larger than the 
  # corresponding lower bounds
  
  if (length(lower)!=length(upper)) stop("number of lower and upper bounds must match")
  # checks to make sure that the user inputs the same number of lower and upper bounds
  
  clusterExport(cl, c("lower","upper"), envir = environment())
  sequences <- list()
  sequences <- parLapply(cl=cl, 1:length(lower), function(x) seq(lower[x],upper[x]-1,by=1))
  gridss <- as.matrix(expand.grid(sequences))
  # create a matrix with the unique combinations of integers from the
  # lower to upper bounds of each unique set
  # in order to do so for higher dimensions, we need to store the sequences for each pair of
  # lower and upper bounds in a list, and then use lapply to extract these sequences.  Then,
  # given the sequences, we create the matrix with expand.grid
  # this replaces the for loop in the first iteration
  
  sp.grid <- createIntegrationGrid( 'KPU', dimension=length(lower), k=5 )
  # create nodes and weights that are used for sparse grid integration;
  # first argument is the type of one-dimensional integration rule;
  # second argument indicates the dimensionality of the integration
  # problem; third argument indicates the accuracy level
  
  clusterExport(cl, c("gridss", "sp.grid"), envir = environment())
  nodes <- parLapply(cl=cl, 1:nrow(gridss), function(x) gridss[x,]+sp.grid$nodes)
  nodes <- do.call(rbind, nodes)
  # initializes our nodes with the first row of gridss plus our nodes from
  # createIntegrationGrid; replaces part of for loop by creating a list of each set of grids,
  # and then rbinding each matrix together
  
  weights <- rep(sp.grid$weights, nrow(gridss))
  # replaces the for loop for weights by simply repeating the weights for the number of
  # rows in gridss
  
  g <- g
  clusterExport(cl, c("g","nodes"), envir = environment())
  gx.sp <- parApply(cl=cl, X=nodes, MARGIN=1, FUN=g) # applies the function g to the nodes
  val.sp <- gx.sp %*%weights # multiplies gx.sp by the weights
  stopCluster(cl) # terminating the parallel cluster
  val.sp # returns val.sp
}

test_func <- function(x){
  x[1]^2+x[2]*3+x[3]^0.5
}

lower <- c(0,1,4)
upper <- c(1,10,8)

sg.mult.par(test_func, lower=lower, upper=upper)
