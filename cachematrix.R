## A cached matrix is a wrapper for a given matrix object (or a new one) that
## caches its inverse on its first calculation.
## Subsequent calls to calculate the inverse may use the cached value instead
## of doing the same calculation over again.
##
## Example usage:
##   m <- matrix(...)
##   cm <- makeCacheMatrix(m)
##   solvecm <- cacheSolve(cm)  # This will perform a first time calculation
##   solvecm <- cacheSolve(cm)  # This will use the cached value
##
##   m2 <- matrix(...)
##   cm$set(m2)  # Set a new matrix, invalidating the cached inverse
##   solvecm <- cacheSolve(cm)  # This will again perform the calculation
##   solvecm <- cacheSolve(cm)  # This will use the cached value


## makeCacheMatrix:
## This function receives a matrix (or creates an empty one if none is given),
## and returns a cache matrix object which caches the inverted matrix when
## calculated.
##
## The returned object has 4 methods:
## - get: Returns the contained matrix.
## - set: Sets the contained matrix to a new value, invalidating cached results.
## - getSolve: Used internally by cacheSolve to retrieve the cached inverse
##             result (can be NULL if no result).
## - setSolve: Used internally by cacheSolve to set the cached inverse result.
makeCacheMatrix <- function(x = matrix()) {
        solveX <- NULL
        set <- function(newX) {
                x <<- newX
                solveX <<- NULL
        }
        get <- function() x
        setSolve <- function(solve) solveX <<- solve
        getSolve <- function() solveX
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}


## cacheSolve:
## This function returns the inverse of the matrix contained within the given
## matrix cache object, as obtained by solve(x) (where x is a matrix).
## If the result was already calculated previously and stored in cache, that
## result is returned.
cacheSolve <- function(x, ...) {
        # Attempt to retrieve the cached value first.
        solveX <- x$getSolve()
        if(!is.null(solveX)) {
                return(solveX)
        }

        # If there was no cached value, compute it.
        data <- x$get()
        solveX <- solve(data, ...)

        # Store the calculated value in the cache.
        x$setSolve(solveX)
        solveX
}
