## makeCacheMatrix sets up the "matrix" that can return the real matrix and its cached inverse, if any. It also has the ability to remember the computed inverse.

makeCacheMatrix <- function(m = matrix()) {
  original_matrix <<- m
  inverse <- NULL
  get_original <- function() original_matrix
  set_inverse <- function(inv) inverse <<- inv
  get_inverse <- function() inverse
  list(get_original = get_original, set_inverse = set_inverse, get_inverse = get_inverse)
}


## Ask the special "matrix" to give us the cached inverse. If there is none, ask for the original, real matrix and compute its inverse. The result is cached and returned.

cacheSolve <- function(matrix, ...) {
  cached <- matrix$get_inverse()
  if(!is.null(cached)) {
    message("returning cache")
    return(cached)
  }
  message("computing inverse")
  original <- matrix$get_original()
  inverse <- solve(original)
  matrix$set_inverse(inverse)
  inverse
}
