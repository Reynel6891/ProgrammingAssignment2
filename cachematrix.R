## I will create a series of functions that allow to retrieve the inverse of a user-defined 
## matrix if previously calculated in the global environment.  


## first, i create a function that calculates and stores in a vector the inverse of any matrix  

makeCacheMatrix <- function(x = matrix()) {
  inverse_1 <- NULL
  set <- function(y) {
    x <<- y
    inverse_1 <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) inverse_1 <<- inverse
  getInverse <- function() inverse_1
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Second, i create a function that search if the inverse of the user-defined matrix
## has been already caculated inside the environment.  

cacheSolve <- function(x, ...) {
  
  inverse_1 <- x$getInverse()
  if (!is.null(inverse_1)) {
    message("cached data")
    return(inverse_1)
  }
  mat <- x$get()
  inverse_1 <- solve(mat, ...)
  x$setInverse(inverse_1)
  inverse_1
}
