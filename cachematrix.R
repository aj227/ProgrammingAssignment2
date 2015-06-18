## The functions below demonstrate the caching functionality 
## within the R programming language. It is particularly useful
## for computationally expensive operations, since it allows for
## storing the results in cache and retrieving it when needed rather
## than computing it repeatedly. The cache functionality is demonstrated
## using the matrix inversion as the operation.

## The function makeCacheMatrix creates a special "matrix" object
## that can store its inverse in cache

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv<<-inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The function cacheSolve takes the "matrix" object created using
## the function makeCacheMatrix and retrieves it's inverse if it
## exists in the cache, otherwise computes the inverse, stores it 
## in the cache and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("getting inverse from cache")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat)
  x$setinv(inv)
  inv
}
