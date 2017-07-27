## Here you find two functions: "makeCacheMatrix" and "cacheSolve".
## The first one sets and gets the value of a matrix and its inverse.
## The second one gives the inverse of a matrix but it is calculated only
## the first time; when it is called again it uses the cached value using
## the function "makeCacheMatrix".

## "makeCacheMatrix" creates a list which contains four functions:
## set: set the matrix
## get: get the matrix
## setinverse: set the inverse of the matrix
## getinverse: get the inverse of the matrix
##
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL 
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve calculate the inverse of the matrix created with the function
## "makeCacheMatrix" and set the value using the function setinverse. 
## Before, it checks if it was already calculated: in this case the value 
## is taken from the cache.
##
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
