## Here are two functions that can be used in combination to cache the
## result of a matrix inversion in order to avoid repeated computation.

## makeCacheMatrix(x) creates a 'cached' version of a matrix x,
## i.e. a list of 'methods' for modifying (set) and retrieving (get)
## x as well as its 'inverse' (setInverse, getInverse).
## In fact there is no control inside makeCacheMatrix what is set
## with setInverse() and will consequently be retrieved with getInverse(),
## so this is just a storage space for 'anything'.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(result) inv <<- result
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## cacheSolve(xCached) takes a 'cached' matrix xCached
## (i.e. a matrix run through makeCacheMatrix())
## and returns its inverse by either calculating it or
## by retrieving the cached inverse if calculated previously.
## the makeCacheMatrix 'methods' setInverse() and getInverse()
## are used to store / retrieve the calculated inverse.

cacheSolve <- function(xCached, ...) {
        ## Return a matrix that is the inverse of 'xCached'
    inverse <- xCached$getInverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- xCached$get()
    inverse <- solve(data, ...)
    xCached$setInverse(inverse)
    inverse
}
