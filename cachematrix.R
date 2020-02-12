## This script is part of an assignment submission
## John Hopkins University and Coursera
## Specialization: Data Science
## Course R Programming
## Assignment type: Peer-graded
## Title: Programming Assignment 2: Lexical Scoping
## Assignment Instructions can be found at:
## https://www.coursera.org/learn/r-programming/peer/tNy8H/programming-assignment-2-lexical-scoping
## Student: Miguel Duarte B.
## email: duartemj@outlook.com

## makeCacheMatrix - This function creates a special "matrix" object that can
##                   cache it inverse.  The function argument is a matrix that
##                   defaults to x = matrix(), or empty matrix.  The function 
##                   returns a matrix object that has the special property of 
##                   stroring its inverse in the cache so the function 
##                   cacheSolve() can use it avoiding calculation if the value
##                   of the inverse is already strored in the cache.
##                   USAGE: y <- makeCacheMatrix(z), where z is a numeric 
##                   matrix and y is a matrix object with the same dimensions
##                   as z.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  # Sets the value of the matrix
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  # Gets the value of the matrix
  get <- function() x
  # Sets the value of the inverse matrix using solve
  setsolve <- function(solve) s <<- solve
  # Gets the value of the inverse matrix
  getsolve <- function() s
  list(set = set,
       get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
}


## cacheSolve - This function computes the inverse of the special "matrix"
##              returned by makeCacheMatrix above.  If the inverse has already
##              been calculated (and the matrix has not changed), then 
##              cacheSolve should retrieve the inverse from the cache.
##              USAGE: first create the special "matrix" object by using:
##              y <- makeCacheMatrix(z), where z is a numeric matrix and y is 
##              a matrix object with the same dimensions as z.  And then use
##              cacheSolve(y) to calculate the inverse only if y changed or
##              the inverse has not been calculated yet and therefore it's not
##              stored in the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  # Checks if the inverse matrix is stored in the cache
  if(!is.null(s)) {
    message("getting cached data")
    # Retruns the inverse matrix from the cache
    return(s)
  }
  # Gets the matrix
  data <- x$get()
  # Calculates the inverse matrix using solve
  s <- solve(data, ...)
  # Sets the inverse matrix
  x$setsolve(s)
  # Returns the inverse matrix just calculated
  s
}
