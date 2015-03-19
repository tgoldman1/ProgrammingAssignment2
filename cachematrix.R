## These functions are used to create an object that stores a matrix and cache the inverse of the matrix.
## The inverse of the matrix is calculated and returend only if it changes. If it does not change, the cached
## inverse of the matrix is returned instead These are used to prevent potentially time-consuming calculations.

## The first function 'makeCacheMatrix' creates a "matrix" object which caches the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverseMatrix <- function(inverseMatrix) m <<- inverseMatrix
  getInverseMatrix <- function() m
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}


## The second function 'cacheSolve' computes the inverse of the matrix object returned by the first 
## function makeCacheMatrix (above) only if the matrix has changed. If the inverse has already been 
## calculated (matrix has not changed), then cacheSolve retrieves the inverse matrix from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverseMatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverseMatrix(m)
  m  
}




