## Put comments here that give an overall description of what your
## functions do
# makeCacheMatrix create an object that can cache a matrix and
# it inverse. makeCacheMatrix has four methods:
# Set: is a method to set the matrix in the cache
# Get: is a method to get the matrix in the cache
# SetInverse: is a method to set the inverse of the matrix in the cache
# GetInverse: is a method to get the inverse of the matrix in the cache
# makeCacheMatrix returns a list of 4 functions 
# Set, Get, SetInverse, GetInverse
#
# cacheSolve is a function to calculate the inverse of a given matrix
# if the inverse of the matrix already been calculated, then cacheSolve
# will get that cached inverse from the object generated from
# makeCacheMatrix


## Write a short comment describing this function
# to create a function which returns a list of 4 functions
# Set, Get, SetInverse, GetInverse

makeCacheMatrix <- function(x = matrix()) {
  xinv <- NULL
  
  # This function sets the matrix and clears the cached inverse
  set <- function(y) {
    x <<- y
    xinv <<- NULL
  }
  
  # This function gets the matrix
  get <- function() {
    x
  }
  
  # This function sets the cached inverse
  setInverse <- function(inverse) {
    xinv <<- inverse
  }
  
  # This function gets the cached inverse
  getInverse <- function() {
    xinv
  }
  
  # Return a list of the functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  xinv <- x$getInverse()
  if (!is.null(xinv)) {
    message("getting cached data")
    return(xinv)
  }
  data <- x$get()
  xinv <- solve(data, ...)
  x$setInverse(xinv)
  xinv
}



