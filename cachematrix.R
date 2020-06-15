## This function creates a special “matrix” object that can cache its inverse.
makeCacheMatrix <- function( x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x<<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse 
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## This is a function than can cache the inverse which is computed by makeCacheMatrix.
## Once the inverse is computed, the function can return the inverse from the cache if the function has
##  has not changed.





cacheSolve <- function(x = matrix()) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(x$get())
  x$setInverse(inv)
  inv
}


