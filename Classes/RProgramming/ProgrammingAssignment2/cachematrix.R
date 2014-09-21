## Caching Computational Expensive Intermediary Results
## makeCacheMatrix: creates support functions for accessing stored intermediary results
## cacheSolve: returns the intermediary result (inverse of matrix) either by calculating it or
##  copying it from memory

## makeCacheMatrix: initializes storage for intermediary result in parent workspace

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setSolve <- function(mat) m <<- mat
  getSolve <- function() m
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


## cacheSolve: returns inverse of matrix submitted in makeCacheMatrix either by direct
## calculation or by copying from memory store.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getSolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  mat <- x$get()
  m <- solve(mat)
  x$setSolve(m)
  m
}
