## The aim of these functions is to firstly create a special matrix object
## that can cache it's inverse. Then a second function is created that 
## will calculate the inverse of the special matrix. If the inverse of the matrix
## has already been calculated it will instead find this in the cache and return it.

## Essentialy what this function does is creates a special matrix that calculates 
## and caches it's inverse

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


## The cachesolve function calculates the inverse of the matrix. However if the
## inverse has already been cached via getinverse then it will find the inverse
## in the cache and not calculate it again

cacheSolve <- function(x, ...) {
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
