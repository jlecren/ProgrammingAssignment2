## cachematrix.R contains a set of functions that can compute and cache the inverse
## of matrices

## makeCacheMatrix returns an object that stores a matrix and
## that can cache its inverse
## x: a matrix
## returns an object containing the cached inversed matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve computes the inverse of a matrix
## and cache it if it was not already computed.
## x: a matrix
## ...: arguments for the function solve(x,...)
## returns the inverse of the matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

## Test the function cacheSolve.

testCacheSolve <- function() {
  
  mdat <- matrix(c(1,0,0, 0,2,0, 1,0,-1), nrow = 3, ncol = 3)
  #minv <- matrix(c(1,0,1, 0,0.5,0, 0,0,-1), nrow = 3, ncol = 3)
  mcache <- makeCacheMatrix(mdat)
  stopifnot( is.null(mcache$getinverse()) )
  cacheSolve(mcache)
  stopifnot( !is.null(mcache$getinverse()) )
  mcache$getinverse()
}
