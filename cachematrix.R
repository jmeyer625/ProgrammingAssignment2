## Pass a matrix to the makeCacheMatrix function, and call cacheSolve to retrieve the inverse of that matrix.
## The first time you call cacheSolve, the solve function will run. If you call it again, the cached value
## will be retrieved with a message indicating that it is the cached value that is being returned.

## This function creates a special matrix object that can store a cached value of its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inverse <<- inverse
  getinverse <- function() inverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function retrieves the cached value of a given matrix's inverse, or calculates it if not already cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if (!is.null(inverse)) {
    message("getting cached value")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
