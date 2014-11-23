library(MASS)

## This function creates a special "matrix" object that can cache its inverse.
## This uses the MASS library ginv function to return inverse Matrices
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(ginv) m <<- ginv
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- ginv(data, ...)
  x$setInverse(m)
  m
}