## Put comments here that give an overall description of what your
## functions do

## Creates an object from a matrix with
## 1. The matrix and (possibly) its inverse (stored in x and inv respectively)
## 2. A set of functions for getting and setting both the matrix and its inverse:
##  set / get: to access and modify the matrix
##  setInv / getInv: to access and modify the matrix's inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(solve) inv <<- solve
    getInv <- function() inv
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## Returns the inverse of the given CacheMatrix.
## If the inverse has not been yet calculated it computes and stores it in the
## CacheMatrix object. Otherwise it just reads the previously computed value and
## returns it directly.

cacheSolve <- function(x, ...) {
    inv <- x$getInv()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInv(inv)
    inv
}
