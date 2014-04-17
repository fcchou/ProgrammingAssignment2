# Matrix inverse solver with caching ability
# Usage:
# 1. Create an invertible matrix 'mat'
# 2. Convert the matrix to a cached form:
#    cache_mat <- makeCahceMatrix(mat)
# 3. Use 'cacheSolve(cache_mat)' to obtain the inverse.
# 4. If cacheSolve are applied to the same cache_mat,
#    a cached copy will be returned.


# Create a cached matrix object as a list.
# Input parameters: a square matrix
# Returns: a list with follwing attributes
# get: get function for cached matrix
# set: set function for cached matrix
# get_inv: get function for cached inverse
# set_inv: set function for cached inverse
# Initially cache_inv = NULL and x = matrix()
makeCacheMatrix <- function(x = matrix()) {
  cache_inv <- NULL
  set <- function(y) {
    x <<- y
    cache_inv <<- NULL
  }
  get <- function() x
  set_inv <- function(inv) cache_inv <<- inv
  get_inv <- function() cache_inv
  list(set=set, get=get, set_inv=set_inv, get_inv=get_inv)
}


# Inverse solver with cache
# Input paramter: a list created by makeCacheMatrix
# Returns: a square matrix, inverse to the matrix stored
#          in the input object
# Extra parameters as in the R 'solve' function can be
# input as well.
cacheSolve <- function(x, ...) {
  cache_inv <- x$get_inv()
  if (!is.null(cache_inv)) {
    message("getting cached data")
    return(cache_inv)
  }
  data <- x$get()
  cache_inv <- solve(data, ...)
  x$set_inv(cache_inv)
  cache_inv
}
