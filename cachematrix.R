## This is a pair of functions designed to cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        ## Create a cache for the inverse
        inv <- NULL
        
        ## Set the value of the special "matrix"
        ## Use <<- operator to store values in a parent environment
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        ## Get the value of the special "matrix"
        get <- function() x
        
        ## Set the value of the inverse of the special "matrix"
        setinverse <- function(inverse) inv <<- inverse
        
        ## Get the value of the inverse of the special "matrix"
        getinverse <- function() inv
        
        ## Specify the list contents
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## This function computes the inverse of the special "matrix" returned by the
## makeCacheMatrix function

cacheSolve <- function(x, ...) {
        ## Retrieve cached inverse
        inv <- x$getinverse()
        
        ## Check the cache, if the inverse is already stored, use it
        ## If the inverse is not stored, skip this step
        if(!is.null(inv)) {
                message("Getting cached inverse")
                return(inv)
        }
        
        ## Call the get function from makeCacheMatrix
        data <- x$get()
        
        ## Compute the inverse
        inv <- solve(data, ...)
        
        ## Cache the result of inverse computation
        x$setinverse(inv)
        
        ## Return result
        inv
}

## Test the functions
## Sample matrix
mat <- matrix(c(2, 1, 1, 2), 2, 2)

## Wrap it in the special cache object
cachedMat <- makeCacheMatrix(mat)

## First call → computes inverse
cacheSolve(cachedMat)

## Second call → retrieves from cache
cacheSolve(cachedMat)

## Change the matrix → cache resets
cachedMat$set(matrix(c(4, 2, 2, 4), 2, 2))
cacheSolve(cachedMat)  # computes new inverse
