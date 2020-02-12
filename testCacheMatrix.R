## This script tests the makeCacheMatrix and cacheSolve scripts for accuracy
## Please clear the environment by clearing the objects from the workspace
## including the hidden objects
## USAGE: testCM(), the output is a data frame with two columns and one row
## The first column shows the fraction of time retrieving the inverse matrix
## from the cache compared to calculating it from scratch.  The expected
## value is a very small number i.e. 0.008771976.
## The second column shows if the identity matrix test was succesful

testCM <- function(
  sFile = "cachematrix.R", 
  mSize = 1e6, 
  useSeed = 1, 
  errM = 0.001) {
  
  # calculate the matrix dimensions
  rcDim <- as.integer(sqrt(mSize))
  
  # Generate a random matrix with the provided seed
  z <- matrix(
    rnorm(mSize,
          mean = 0,
          sd = useSeed),
    nrow = rcDim,
    ncol = rcDim
  )
  
  # Source the script
  source(sFile)
  
  # Generate a "matrix" object for matrix z using makeCachematrix function
  # and store it in the variable y
  y = makeCacheMatrix(z)
  
  # Calculate the inverse matrix of z using cacheSolve function, then calculate
  # and store the execution time
  firstStart <- Sys.time()
  invisible(cacheSolve(y))
  firstEnd <- Sys.time()
  firstExec <- firstEnd - firstStart
  
  # Calculate the inverse matrix of z using cacheSolve function, store it in
  # the variable inv_z, also recording the execution time
  secondStart <-Sys.time()
  inv_z <- cacheSolve(y)
  secondEnd <- Sys.time()
  secondExec <- secondEnd - secondStart
  
  # Calculate the time fraction in the second run in comparison to the first one
  # This will help understand if the second run actually used the cache instead 
  # of recalculating the inverse matrix
  timeFraction <- as.numeric(secondExec) / as.numeric(firstExec)
  
  # Using the inverse and identity matrices properties, calculate the identity 
  # matrix by applying matrix multiplicaton on z and inv_z
  test_iden <- z %*% inv_z
  
  # test_iden should be an identity matrix size rcDim X rcDim, therefore its sum
  # should be rcDim.  Test this condition to check if the inverse matrix was
  # correctly calculate
  passTest <- abs(
    sum(test_iden) -
      rcDim) <= errM
  
  data.frame("Time Fraction" = timeFraction, "Passed Inv Test" = as.logical(passTest))
  
}