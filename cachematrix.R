## This function creates a special "matrix" object that can cache its inverse. 
## Specifically, makeCacheMatrix creates a list with a function to (1) set the 
## value of the matrix; (2) get the value of the matrix; (3) set the value of 
## the inverse of the matrix; (4) get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() {
    x
  }
  setinv <- function(inverse) {
    inv <<- inverse
  }
  getinv <- function() {
    inv
  }
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. cacheSolve first checks whether the inverse has 
## already been calculated using the getinv() function within the object
## defined by makeCacheMatrix; if so, the cacheSolve retrieves the inverse from 
## the cache and prints a message.

## If the inverse of the matrix has not been calculated (its value is NULL),
## then the solve() function is used to calculate the inverse; this value is 
## then returned by cacheSolve.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv 
}
