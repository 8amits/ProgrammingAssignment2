## Assignment: Caching the Inverse of a Matrix
## Here, we have written two functions named as 'makeCacheMatrix' & 'cacheSolve' that cache 
## the inverse of matrix.

## 1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse. 
##   This object is really a list containing function to a) set the value of the matrix, 
##   b) get the value of the matrix, 
##   c) set the value of inverse matrix, 
##   d) get the value of inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  }
  
## 2. cacheSolve: This function calculates the inverse of the special "matrix" created with the 
## 'makeCacheMatrix' function. It first checks to see if the inverse matrix has already been calculated. 
## If so, it gets the inverse matrix from the cache and skips the computation. Otherwise, it calculates 
## the inverse matrix of the data and sets the value of the inverse matrix in the cache via the setinverse
## function. 

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
  }