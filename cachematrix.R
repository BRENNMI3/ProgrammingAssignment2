
## This pair of functions cache the inverse of a matrix


# The function makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(mt = matrix()) {
        
        mt_inv <- NULL
        set <- function(y) {
                mt <<- y
                mt_inv <<- NULL
        }
        get <- function() mt
        setinv <- function(inv) mt_inv <<- inv
        getinv <- function() mt_inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)    
        
}


# The function cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        
        mt_inv <- x$getinv()
        if(!is.null(mt_inv)) {
                message("getting cached data")
                return(mt_inv)
        }
        data <- x$get()
        mt_inv <- solve(data,...)
        x$setinv(mt_inv)
        mt_inv
}