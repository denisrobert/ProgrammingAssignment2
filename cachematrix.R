## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  
  set <- function(mx) {
    x <<- mx
    invMatrix <<- NULL
  }
  get <- function() x
  
  setinv <- function(invm) invMatrix <<- invm
  getinv <- function() invMatrix
  
  list(get=get, set=set, getinv=getinv, setinv=setinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("returning cached inverse")
    return(inv)
  }
  
  inv <- solve(x$get(), ...)
  x$setinv(inv)
  
  inv
}
