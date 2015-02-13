## Pair of routines to provide for optimization of Matrix Inversion operations
## by caching results of inversions so that subsequent calls will avoid re-calculation

## Function to create a wrapper construct around a matrix that also contains
## a location to store the cached inversion result. 

makeCacheMatrix <- function(m = matrix()) {
        i <- NULL
        set <- function(y) {
                m <<- y
                i <<- NULL
        }
        get <- function() m
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Routine to return the inversion of the supplied Matrix (which MUST have earlier been 
## supplied to makeCacheMatrix) and to cache result for return on subsequent calls.

cacheSolve <- function(m, ...) {
        ## Return a matrix that is the inverse of 'm'
        i <- m$getinv()
        if(!is.null(i)) {
                return(i)
        }
        data <- m$get()
        i <- solve(data, ...)
        m$setinv(i)
        i
}
