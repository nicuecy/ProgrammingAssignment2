# This file contains the functions:
#       makeCacheMatrix - creates a matrix object that can cache its inverse
#       cacheSolve      - computes the inverse of a matrix object returned by makeCacheMatrix
# Code is based on the example supplied for this assignment
# Last modified 18.1.15

# This function creates a special "matrix" object that can cache its inverse.
# The function creates a list of functions to:
#       set the value of the matrix
#       get the value of the matrix
#       set the inverse of the matrix
#       get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(i) inverse <<- i
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
# retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
