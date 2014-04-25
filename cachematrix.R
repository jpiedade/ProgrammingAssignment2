## This function creates a "speacial matrix' that is really a list
## containing functions that:
##    sets the value of the matrix
##    gets the value of the matrix
##    sets the value of the inverse matrix
##    gets the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function returns a matrix that is the inverse of 'x'
## First this function tries to get the value of the solve 
## for the input matrix 'x'.
## If the inverse of the matrix 'x' is already cached it returns
## the value that is stored on the cache.
## If not it calculates the inverse of 'x' and adds the calculated 
## value to the cache storage.
## Finally the function prints the value of the inverse of 'x'.

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
