## The following functions will create a matrix which can cache its inverse 
## and retrieve that cached inverse if the matrix is unchanged.


## Use this matlib for matrix transformations

library(matlib)


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      I <- NULL
      set <- function(y) {
            x <<- y
            I <<- NULL
      }
      get <- function() x
      setinverse <- function(inv) I <<- inv
      getinverse <- function() I
      list(set = set, get = get,
           setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special
##"matrix" returned by `makeCacheMatrix` above. If the inverse has
##already been calculated (and the matrix has not changed), then
##`cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      I <- x$getinverse()
      if(!is.null(I)) {
            message("getting cached inverse of matrix")
            return(I)
      }
      data <- x$get()
      I <- inv(data, ...)
      x$setinverse(I)
      I
}


## This is a test of the functions

x <- matrix(c(2,3,4,
              5,6,7,
              1,2,5), nrow=3, byrow=TRUE)
x
det(x)
xx <- makeCacheMatrix(x)
cacheSolve(xx)

## Success!
