## cachematrix is used to speed processing time when  
## repeatedly computing the inverse of a matrix.
## Usage: Use makeCacheMatrix() to initially store a matrix
## and cacheSolve() to compute the matrix inverse.
## After the initial invocation cacheSolve() will return 
## the cached inverse rather than perform the caculation.

## MakeCacheMatrix() is used to store a matrix and the
## matrix inverse along with the functions (set, get,
## setInverse, getInverse) used to get or set the matrix
## or matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(matrix) m <<- matrix
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## cacheSolve() is used to solve the inverse of a matrix
## created with makeCacheMatrix(). If the inverse has
## already been caculated cacheSolve retrives it from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached inverse of matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
