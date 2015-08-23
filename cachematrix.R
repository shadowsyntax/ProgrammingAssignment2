## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function (y) {
        x <<- y
        i <<- NULL
    }
    # Function to get the matrix 'x' and return its value.
    get <- function() {
        x
    }
    # Funcitn to set the inverse of matrix 'x'.
    setInverse <- function(solve) {
        i <<- solve
    }
    # Function to get the inverse of matrix 'x' from cache.
    getInverse <- function() {
        i
    }
    # Returns a list of functions.
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by the makeCacheMatrix function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        # Checks the cache for already computed inverse of matrix 'x'
        if(!is.null(i)) {
            message("Getting cached data")
            return(i)
        }
        # Computes the inverse of matrix 'x' if cache is empty and sets the computed inverse in cache.
        data <- x$get()
        i <- solve(data)
        x$setInverse(i)
        i
}
