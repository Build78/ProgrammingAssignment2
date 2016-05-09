## Two functions to make it possible to cache inverted matrix results.
## A usage example:
## var.a <- makeCacheMatrix(matrix(c(2, 4, 3, 1, 5, 7, 7, 4, 6), nrow=3, ncol=3))
## cacheSolve(var.a) ## Executes the inverse
## cacheSolve(var.a) ## Returns the cached version

## Use this function to create a cacheable inverted matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Cache a inverted matrix based on the makeCacheMatrix
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


