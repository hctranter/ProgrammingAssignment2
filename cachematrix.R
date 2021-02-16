## The purpose of this assignment is to write a pair of functions that cache the
## the inverse of a matrix. 

## makeCacheMatrix creates a special "matrix" object that can cache its inverse 
## or store the matrix and its inverse.
## The special "matrix" object created by makeCacheMatrix is a list of functions
## along with the matrices x and m (the inverse of x,if already cached).
## that are accessible in the parent environment.
makeCacheMatrix <- function(x = matrix()) {
        m <-NULL
        set <-function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <-function(inverse) m <<- inverse
        getinverse <- function() m
        list(set =set, get=get,
             setinverse =setinverse,
             getinverse =getinverse)
}

## cacheSolve takes an argument that is returned by makeCacheMatrix.cacheSolve
## calculates the inverse of the matrix returned by makeCacheMatrix. If the 
## inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.
## cacheSolve retrieves the inverse from the cached value that is stored in the
## makeCacheMatrix() object's environment. 

cacheSolve <- function(x, ...) {
        m <-x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
