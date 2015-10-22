## functions:
##   makeCacheMatrix: creates a cacheable matrix from a matrix and stores it in a cache
##   cacheSolve: inverts a matrix if not in cache yet, otherwise takes inverted matrix from cache

## creates a special matrix which can be stored in a cache.
## a cache itself is simple key-value, so a list will do as a cache, even if we
##   store a matrix as an object.
## inside the cache, we store the inverse of the matrix, which is calculated
##   using the solve() function.
## we assume the matrix is invertable, so there are no validity checks on this part

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inverse <<- solve
    getinverse <- function() inverse
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## inverts a matrix if not in cache yet, otherwise takes inverted matrix from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
