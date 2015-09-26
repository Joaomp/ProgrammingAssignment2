## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix() ) {
        # at the begining the inverse is not yet cahed
        inv <- NULL
        
        # defining the set function
        set <- function(m) {
                x <<- m
                inv <<- NULL # each time we set the matrix the cache becomes invalid
        }
        
        get <- function() x # just return the matrix
        
        # set a new result in the cache
        setinverse <- function(inverse) inv <<- inverse 
        
        # just return the cahed value
        getinverse <- function() inv 
        
        # return the list of functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Write a short comment describing this function
# This function computes the inverse of the special "matrix" returned by
# makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed),
# then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # get the cached value
        inv <- x$getinverse()
        
        # and if the cached is not empty just return the cached value
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        # If we still here, it means that we need to compute a new value
        # and store it in the cache 
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}