## These function allow for cacheing of inverse matrices.
## makeCacheMatrix creates a list which defines accessors for a 
##  matrix and its inverse, while cacheSolve calculates the inverse
##  and stores the inverse so that the calculation is only done once.

## Creates an object-like list with four methods, encapsulating a matrix and its cached inverse:
##   get,set: accessors for the matrix
##   getinv,setinv: accessors for the matrix' inverse

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


## Returns the inverse of a matrix encapsulated via makeCacheMatrix and cache it for future use.
##  If the inverse has already been calculated, return the cached value rather than recalculate it.

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
