## These two functions are to fulfill the programming assignment 2
## requirement for the R-programming course.

## The makeCacheMatrix function creates a matrix object that can cache
## its inverse.  It is based on the programming assignment stub used
## for our class.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse_m) m <<- inverse_m
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The cacheSolve function computes the inverse of the matrix
## returned by the makeCacheMatrix function.  If the inverse
## already exists and the matrix has not changed cachesolve will
## retrieve the inverse from the cache and print a message indicating
## that it is using the cache to get the inverse.  If not, the inverse
## is found using the solve function in R.

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
