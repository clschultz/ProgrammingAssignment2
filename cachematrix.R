## The following functions facilitate calculation of the inverse of a square 
## matrix.  If the value has already been calculated the resulting inversion 
## matrix is pulled from a cache store.  This function assumes that the matrix
## is always invertable, and by this assumption, the matrix is square


## makeCacheMatrix - creates a special matrix with a list of functions to:
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the value of the inverse matrix
## 4) get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setSolve <- function(solve) m <<- solve
    getSolve <- function() m
    list(set = set, get = get, 
         setSolve = setSolve,
         getSolve = getSolve)
}


## cacheSolve - returns the inverse of square matrix.  If the inverse has
## already been calculated it is pulled from the cache store

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getSolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setSolve(m)
    m
}
