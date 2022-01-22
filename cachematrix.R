## The functions defined below compute the inverse of the matrix object and
## store in the cache.

## makeCacheMatrix takes matrix as a parameter and creates a matrix object that
## stores the inverse of the matrix as cache 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrixinverse <- function(inverse) m <<- inverse
        getmatrixinverse <- function() m
        
        list(set = set, get = get,
             setmatrixinverse = setmatrixinverse,
             getmatrixinverse = getmatrixinverse)
}


## This function checks if the inverse for parameter matrix object is cached
## If the inverse is same as cached inverse matrix object (and matrix is not
## changed), then `cacheSolve` should return the retrieved inverse matrix 
## from the cache. Otherwise compute the inverse of the matrix object 
## returned by makeCacheMatrix, return it and cache it for future.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrixinverse()
        
        ## if inverse from cache is not null then return cached inverse
        if(!is.null(m)) {
                message("Matrix inverse found in cache...so fetching")
                return(m)
        }
        
        ## get the matrix object and solve to get the inverse of the matrix
        matrixdata <- x$get()
        m <- solve(matrixdata)
        
        ## set the inverse matrix in the cache
        x$setmatrixinverse(m)
        m
}
