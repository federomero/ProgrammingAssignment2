## These functions will allow users to cache the result of calculating the
## inverse of a matrix.

## makeCacheMatrix returns a list of functions to set and get the result of a
## calculation perfomed on a given matrix
makeCacheMatrix <- function(x = matrix()) {
    s <- NULL

    # Function to set the data and clear the previous cache
    set <- function(y) {
        x <<- y
        s <<- NULL
    }

    # Function to get the data
    get <- function() x

    # Function to set the cache
    setsolve <- function(solve) s <<- solve

    # Function to get the cache
    getsolve <- function() s

    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## cacheSolve calculates the matrix's inverse (if it hasn't been already
## calculated) and caches the result
cacheSolve <- function(x, ...) {
    # Retrieve calculation from cache and return it if present
    s <- x$getsolve()
    if(!is.null(s)) {
        return(s)
    }

    # Perform calculation on data, store the result in the cache for later use
    # and return the result
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}
