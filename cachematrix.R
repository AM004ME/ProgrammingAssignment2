## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inverse_matrix <- NULL
  set <- function(y) {
    x <<- y
    inverse_matrix <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inverse_matrix <<- inverse
  getInverse <- function() inverse_matrix
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  inverse_matrix <- x$getInverse()
  if (!is.null(inverse_matrix)) {
    message("getting cached data")
    return(inverse_matrix)
  }
  mat <- x$get()
  inverse_matrix <- solve(mat, ...)
  x$setInverse(inverse_matrix)
  inverse_matrix
}
