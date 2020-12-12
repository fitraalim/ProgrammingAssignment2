## Programming Assignment 2: Lexical Scoping
## The goal is to reduce computations using cache

## Using matrixcalc package to ease matrix singularity check
## Install and load matrixcalc package
if (!require('matrixcalc')) install.packages('matrixcalc'); library('matrixcalc')

## Avoid getting reached getOption("max.print") message
options(max.print=9999)

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache. It uses is.singular.matrix for singularity checking.
cacheSolve <- function(x, ...) {
  ## Check whether the matrix is singular or not
  if (is.singular.matrix(myMatrix$get()) == TRUE) {
    message("Matrix is singular. Stopping.")
  } else {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    ## Was it calculated? If so, skip calculation.
    if (!is.null(inv)) {
      message("Getting Cached Data")
      return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
  }
}
