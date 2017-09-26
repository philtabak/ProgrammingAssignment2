## This file contains a pair of functions to cache and retrieve the inverse of a matrix.
## These functions can be used to save computing time for large matricies that will not change.


## makeCacheMatrix builds on object containing the inverse of a given matrix.
## It is not very useful on it's own, it needs help from it's friend cacheSolve to do anything interesting.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setcachematrix <- function(solve) m <<- solve
  getcachematrix <- function() m
  list(set = set, get = get,
       setcachematrix = setcachematrix,
       getcachematrix = getcachematrix)
}


## cacheSolve returns a cached matrix (inverse of supplied matrix) if it exists. If it does not exist, it will be created and returned.
## Subsequent calls will return the cached matrix, if this is the case, a diagnostic message of "getting cached data" will be called. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getcachematrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setcachematrix(m)
  m
}
