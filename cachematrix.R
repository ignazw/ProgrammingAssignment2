## functions in this file:
##   makeCacheMatrix:
##       creates a cacheable matrix from a matrix and stores it in a cache
##   cacheSolve:
##       inverts a matrix if not in cache yet, otherwise takes inverted matrix
##       from cache

## function makeCacheMatrix
##      Creates a special matrix which can be stored in a cache.
##      A cache itself is simple key-value, so a list will do as a cache, even
##      if we store a matrix as the R object.
##      Inside the cache, we store the inverse of the matrix, which is calculated
##      using the solve() function.
## arguments:
##      x: is a matrix
##         We assume the matrix is invertable, so there are no validity checks
##         on this part

makeCacheMatrix <- function(x = matrix()) {

    ## make sure we initialise the inverse of the matrix to NULL
    inverse <- NULL

    ## set is a locally scoped function that sets the value of the cacheMatrix
    ## to the original matrix, and reinitialises the inverse to NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }

    ## get is a locally scoped function which retrieves the original matrix.
    ## This will return NULL as long as the set function has not been called.
    get <- function() x

    ## setinverse is a locally scoped function which calculates the inverse
    ## of the original matrix using the global solve() function
    setinverse <- function(solve) inverse <<- solve

    ## getinverse is a locally scoped function which retrieves the inverse
    ## of the original matrix. This will return NULL as long as setinverse
    ## function has not be called
    getinverse <- function() inverse

    ## store each function result in a list. This is basically our cache.
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## function cacheSolve
##      inverts a matrix if not in cache yet, otherwise takes the inverted matrix
##      from the cache
## arguments:
##      x: is a cacheable matrix, created using the makeCachceMatrix function
##         We assume the matrix is invertable, so there are no validity checks
##         on this part
##      ...: any other arguments that will be passed on to the global solve()
##           function

cacheSolve <- function(x, ...) {

        ## retrieve the inverse from the cacheable matrix
        inverse <- x$getinverse()

        ## check if the inverse was already calculated, which means it is not
        ## NULL. If it was calculate, return the inverse value.
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }

        ## the inverse was not calculated yet, so we have to do that now.
        ## first get the original matrix data
        data <- x$get()

        ## next calculate the inverse using the global solve() function.
        inverse <- solve(data, ...)

        ## store the result in the cacheable matrix, for later usage.
        x$setinverse(inverse)

        ## finally return the inverse of the matrix 'x'
        inverse
}
