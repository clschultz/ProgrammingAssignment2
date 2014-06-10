## The following functions facilitate calculation of the inverse of a square 
## matrix.  If the value has already been calculated the resulting inversion 
## matrix is pulled from a cache store.  This function assumes that the matrix
## is always invertable, and by this assumption, the matrix is square

## USAGE:
## 1 create special cache matrix
##      valueName <- makeCacheMatrix(matrix())
## 2 solve using cacheSolve
##      cacheSolve(valueName)

## EXAMPLE:
## sets value to simple square matrix, calculates inverse, then proves 
## correctness by m * m^-1 = I
##      > value <- makeCacheMatrix(matrix(1:4, nrow=2))
##      > cacheSolve(value)
##           [,1] [,2]
##      [1,]   -2  1.5
##      [2,]    1 -0.5
##      > cacheSolve(value)
##      getting cached data
##           [,1] [,2]
##      [1,]   -2  1.5
##      [2,]    1 -0.5
##      > value$get() %*% value$getSolve()
##           [,1] [,2]
##      [1,]    1    0
##      [2,]    0    1
    
## makeCacheMatrix - creates a special matrix with a list of functions to:
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the value of the inverse matrix
## 4) get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    # set value of matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    # get value of matrix
    get <- function() x
    # set the solution for inverse
    setSolve <- function(solve) m <<- solve
    # get the solution for the inverse
    getSolve <- function() m
    # create list of functions available to makeCacheMatrix object
    list(set = set, get = get, 
         setSolve = setSolve,
         getSolve = getSolve)
}


## cacheSolve - returns the inverse of square matrix.  If the inverse has
## already been calculated it is pulled from the cache store

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    # attempt to retrieve an already solved value
    m <- x$getSolve()
    # if the already solved value exists (not NULL) return the solved value
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    # get the matrix from the makeCacheMatrix object
    data <- x$get()
    # get the inverse using the solve function
    m <- solve(data, ...)
    # set the solved matrix into the makeCacheMatrix object
    x$setSolve(m)
    # return the solved matrix
    m
}
