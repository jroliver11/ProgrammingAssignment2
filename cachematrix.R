
## cache function that stores the matrix and the inverse matrix 

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL                                ##create an object inv
     set <- function(y){                        ##find the actual matrix
          x <<- y                               
          inv <<- NULL
     }
     get <- function() x                        ##set the actual matrix
     setInverse <- function() inv <<- solve(x)  ##set the inverse if not already
     getInverse <- function() inv               ##get the inverse
     list(set = set,
          get = get,
          setInverse = setInverse,
          getInverse = getInverse)
}


## accepts a matrix, checks if the inverse has already been calculatd, and if so
## returns it.  Otherwise it calculates the inverse and stores it using
## makeCacheMatrix

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     inv <- x$getInverse()                       ##use the makeCacheMatrix func
     if (!is.null(inv)) {                        ##if we already have the
          return(inv)                            ## the inverse, return it
     }
     mat <- x$get()                              ##get the matrix
     inv <- solve(mat, ...)                      ##solve for the inverse if NULL
     x$setInverse(inv)                           ##cache the inverse
     inv                                         ##return the inverse
}