## Functions to create and use "matrix" objects that can cache their inverse matrix.
## Such an object has functions to store and retrieve a matrix, as well as the inverse of the matrix.
## This inverse is only computed once, and then cached.
## Subsequent calls to get the inverse return the cached value, unless the matrix changes,
##     after which the first request for the inverse will again compute the new matrix's inverse once, and cache it.

## This function creates a special "matrix" object that can cache its inverse.
## It is really a list containing functions to
##     set the value of the matrix
##     get the value of the matrix
##     set the value of the inverse
##     get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setinverse <- function(solve) m <<- solve
     getinverse <- function() m
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## The following function calculates the inverse of the special "matrix" created with the above function.
## However, it first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache
##     via the setinverse function.

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     m <- x$getinverse()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setinverse(m)
     m
}
