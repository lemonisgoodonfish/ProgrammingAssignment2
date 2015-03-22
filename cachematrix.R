## Put comments here that give an overall description of what your
## functions do

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


## Write a short comment describing this function

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
