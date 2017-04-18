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
  
  # sequence <- NULL
  # for(i in 1:length(upper)){
  #   sequence <- expand.grid(sequence, seq(lower[i],upper[i]-1,by=1))
  # }
  
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

test_func <- function(x){
  x[1]^2+x[2]*3
}

sg.int(test_func, lower=lower, upper=upper)

#############################

# now making the function multidimensional

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
  
  sp.grid <- createIntegrationGrid( 'KPU', dimension=length(lower), k=5 )
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

test_func <- function(x){
  x[1]^2+x[2]*3
}

sg.int(test_func, lower=lower, upper=upper)