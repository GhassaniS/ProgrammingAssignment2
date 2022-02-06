## Put comments here that give an overall description of what your
## functions do

## Creating a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    makeVector(x = numeric()) {
      inv <- NULL
    }
    ##Setting the value of the vector
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    ##Getting the value of the vector
    get <- function() x
    
    ##Setting the value of the inverse
    setInverse <-function(solveMatrix) inv <<- solveMatrix
    
    ##Getting the value of the inverse
    getInverse <- function() inv
    list(set = set, get = get, setInverse = setInverse, getInverse=getInverse)
}


## Computing the inverse of the matrix returned from makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          inv <- x$getInverse()
          if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
          }
          data<-x$get()
          inv <- Solve(data,...)
          x$setInverse(inv)
          inv
  }
