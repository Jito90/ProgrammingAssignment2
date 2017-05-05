## Put comments here that give an overall description of what your
## functions do

## Creating a matrix that returns a list of functions to get the inverse of a function

makeCacheMatrix <- function(x = matrix()) {
       
       i <- NULL
       set <- function(y) {
              x <<- y
              i <<- NULL
       }
       get <- function() x
       setInverse <- function(cache) i <<- cache
       getInverse <- function() i
       list(set = set, get = get, setInverse = setInverse,
            getInverse = getInverse)

}


## creating a function to return the inverse of a matrix using GINV funtion from library matrixcalc.
## If the cache exists, it will return the cached value

cacheSolve <- function(x, ...) {
       library(matrixcalc)
       
       i <- x$getInverse()
       if(!is.null(i)) {
              message("getting cached data")
              return(i)
       }
       data <- x$get()
       i <- ginv(data)
       x$setInverse(i)
       i

}
