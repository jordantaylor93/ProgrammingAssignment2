## This script enables creation of a matrix, the inverse of which can be calculated and 
## cached


## Assigns a supplied matrix to get() and caches an externally defined/computed
## inverse in setinverse() 
makeCacheMatrix <- function(a = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() a
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(get = get, setinverse = setinverse, getinverse = getinverse)
}

## Returns the inverse of a matrix supplied to makeCacheMatrix - either a pre-defined 
## inverse present in the cache, or if no cached inverse is present, one which cacheSolve 
## itself computes and places in the cache
cacheSolve <- function(x, ...) {  
  i <- x$getinverse()
  if(!is.null(i)) {
    message("Inverse matrix from cache:")
    return(i)
  }
  matrix <- x$get()
  i <- solve(matrix)
  x$setinverse(i)
  message("Computed inverse matrix:")
  return(i)
}
