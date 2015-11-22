## The below described functions is created for caching the inverse of a matrix
## in order to avoid repeatedly computations of matrix inversion.
## Below you may find the functions that create a special object to store 
## a matrix and caches its inverse information.

## This function allow to create a certain object that can cache the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## The below function allows to compute the inverse of the matrix that's created by above
## 'makeCacheMatrix' function. In case of inverse function is calculated, then it should extract
## the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}
