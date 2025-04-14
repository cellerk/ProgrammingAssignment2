## The functions below can be used together to cache the inverse of a matrix.
## Given that matrix inversion is a costly computation, there may be some benefit
## to caching the inverse of a matrix rather than computing it each time it is needed.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
            x <<- y
            m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## cacheSolve computes the inverse of the special "matrix" created by makeCacheMatrix.
## If the inverse can already been calculated, then cachesolve retrieves the inverse
## from the cache. If not, it solves for the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
