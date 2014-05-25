## Implements matrices which cache their inverses to speed computation

## Creates a cacheMatrix, which stores its inverse and can have this
## both set and retrieved. The matrix input is assumed to be square.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Returns the inverse of the cached matrix. If the inverse is 
## already calculated and available, uses the cached inverse 
## rather than recomputing.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  xMatrix <- x$get()
  inv <- solve(xMatrix, ...)
  x$setInverse(inv)
  inv
}
