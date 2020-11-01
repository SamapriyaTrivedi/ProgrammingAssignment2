## Caching the Inverse of a Matrix
## A R function which is able to cache potentially time-consuming computations.

## Creating a matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  get <- function() {x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Computing the inverse of the matrix returned by above function
cacheSolve <- function(x, ...){
  inv <- x$getInverse()
  
  ## If the inverse has already been calculated
  ## Then cacheSolve should retrieve the inverse from the cache.
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
