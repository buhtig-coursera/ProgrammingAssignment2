## This file contains two functions for managing a cache 
## for the calculation of matrix inverses

## makeCacheMatrix returns a list of functions for setting/getting
## the matrix and the matrix inverse. The matrix can optionally be 
## set during the initial call to makeCacheMatrix. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function(){ x }
  setInv <- function(y){
    inv <<- y
  }
  getInv <- function(){ inv }
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## cacheSolve takes the list of functions created by 
## makeCacheMatrix and returns the inverse by using the
## cached value if it has already been set. If not, it 
## calculates the inverse and stores it back to the cache 
## and returns the value. Additional function arguments 
## are passed through to the solve function.

cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if(!is.null(inv)){
    message("returning inverse from cache")
    return(inv)
  }
  cacheMatrix <- x$get()
  inv <- solve(cacheMatrix, ...)
  x$setInv(inv)
  inv
}
