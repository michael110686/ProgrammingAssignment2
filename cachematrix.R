## Put comments here that give an overall description of what your
## functions do

## The following function (makeCacheMatrix) creates a special "matrix",
## which is a list containing four functions to:
##     1. set the value of the matrix (variable x)
##     2. get the value of the matrix (variable x)
##     3. set the value of the inverse matrix (variable inv)
##     4. get the value of the inverse matrix (variable inv)

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function()
        x
    setinv <- function(solve)
        inv <<- solve
    getinv <- function()
        inv
    list(
        set = set,
        get = get,
        setinv = setinv,
        getinv = getinv
    )
}


## The following function returns the inverse of the special "matrix" created 
## with the above function (makeCacheMatrix).
## However, it first checks to see if the inverse has already been created
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it creates the inverse of the matrix using the solve-function 
## and sets it in the cache via the setinv function.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
