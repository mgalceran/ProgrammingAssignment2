## This file contains two functions that can be used to compute 
## the inverse of a matrix. If the inverse of the matrix has been
## already calculated, the cached value will be returned
## Otherwise, it will be computed, cached and returned

#  create a special matrix, a list with functions to set/get the value of the matrix, and get/set the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    mat_inverse <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


# return the inverse of a matrix. If cached, return the cached inverse, otherwise, compute using the solv function
cacheSolve <- function(x, ...) {
  solv <- x$getsolve()
  if(!is.null(solv)) {
    message("getting cached data")
    return(solv)
  }
  data <- x$get()
  solv <- solve(data, ...)
  x$setsolve(solv)
  solv
}
