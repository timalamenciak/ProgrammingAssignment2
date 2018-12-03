## This script contains two functions to help cache the inverse of a matrix.
## 
## makeCacheMatrix takes a matrix as an input
## and returns a list of functions to work with
## that matrix.
##
## cacheSolve takes the output of makeCacheMatrix 
## and checks if the inverse of the matrix has already
## been calculated. If it has, it retrieves and outputs it
## If it has not, it calculates the value of the matrix and
## caches it.

##This function returns an object that can work on the matrix.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) x <<- y
  get <- function() x
  setinverse <- function(y) i <<- solve(x)
  getinverse <- function() i
  list(set = set, 
       get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}

## This function accepts the input of makeCacheMatrix and returns
## the inverse. It checks if the inverse has already been cached and
## if not, calculates it.

cacheSolve <- function(x, ...) {
  c <- x$getinverse()
  if (!is.null(c)) {
      return(c)
  } 

  c <- solve(x$get())
  x$setinverse(c)
  c
}
