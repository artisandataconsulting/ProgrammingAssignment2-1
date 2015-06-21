## cachematrix.R implements Programming Assignment 2 to cache the calculation results.

## makeCacheMatrix will create a "matrix" wrapper "objects" that has
## access to computed/stored inverse
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    ## whenever set is called, reset the reference to x (the matrix)
    ## and the inverse if any.
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() {
        x
    }
    setInverse <- function(i) {
        inverse <<- i
    }
    getInverse <- function() {
        inverse
    }
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Checking to see if inverse has already been computed, if yes,
##   returned the computed inverse if not, use solve to compute it,
##   before returning, cache it
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    if (!is.null(inverse)) {
        message("returning previously calculated inverse")
        return (inverse)
    }
    matrix <- x$get()
    inverse <- solve(matrix, ...)
    x$setInverse(inverse)
    ## return the calculated result after caching it
    inverse
}
