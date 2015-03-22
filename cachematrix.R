## makeCacheMatrix provides a wrapper around an ordinary matrix that allows
## an inverse result to be cached.
## cacheSolve will compute the inverse of an instance of makeCacheMatrix. If
## there is no cached value, it will run solve and return the result, saving
## it to the cache in the matrix. Next time it is called, it will use the
## cached result if it is not null.
## Note : calling set(y) on makeCacheMatrix will null the cache value
## and cause cacheSolve to recalculate the inverse.

## Special matrix which can be used to cache the value of an inverse.
## use setinverse to set the value to be cached

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverseData) inv <<- inverseData
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## find the inverse of a special cacheMatrix and cache the result
## in the instance of that matrix if it has not been calculated
## before. Note: x must support the methods of makeCacheMatrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'      
  dataInverse <- x$getinverse()
  if(!is.null(dataInverse)) {
    message("getting cached data")
    return(dataInverse)
  }
  data <- x$get()
  dataInverse <- solve(data, ...)
  x$setinverse(dataInverse)
  dataInverse
}
