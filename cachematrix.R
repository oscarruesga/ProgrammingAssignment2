## Obtain the inverse of a square matrix through
## a cache funtion that invoke the solve function
## Use makeCacheMatrix to prepare the matrix to be
## cached through the cacheSolve function

## Prepare a square matrix to be cached through
## the cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve)s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function invert the matrix through the
## solve funtion. If the matrix previously invoke
## this method return the cache value of invert value
## if not invoke to the solve method to invert the
## matrix and store the result in a cache object
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  if(!is.null(s)) {
    message("Getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data,...)
  x$setsolve(s)
  s
}


