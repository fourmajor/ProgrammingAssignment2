## these two functions provide methods to store and retrieve a matrix as well as
## its inverse. A cache of the inverse is kept so it doesn't need to be computed
## every time.

## makeCacheMatrix can get and set the matrix as well as the cache of its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
  }
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve checks the cache of x to see if the inverse has already been solved.
## if already solved it returns the cache. otherwise it computes the inverse and
## stores it in x's cache
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv))
  {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
