## tim.keck@gmail.com 
## R Programming JHU Jan 2015
##
##  A set of functions to optimize calculating the inverse
## of a matrix by caching the calculated value.

## `makeCacheMtrix` creates a special "matrix", which is
## really a list containing functions to
##  1.  set the value of the matrix
##  2.  get the value of the matrix
##  3.  set the value of the inverse
##  4.  get the value of the inverse

  
  makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  }

## The following function returns the inverse of the special "matrix"
## created with the above function. However, it first checks to see if the
## inverse has already been calculated. If so, it returns the cached inverse 
## from the cache and skips the computation. 
## Otherwise, it calculates the inverse of
## the data and sets the value of the inverse in the cache via the 
## `setinverse` function.

## Returns a matrix that is the inverse of 'x'


cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
