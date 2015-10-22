## The makeCacheMatrix function simply stores an inversed matrix into memory simulating a "cache".
## This function implements the following commands:
##     get          : returns the original matrix
##     getinverse   : returns the inversed matrix (NULL if no inversed matrix available)
##     setinverse   : store inversed matrix into "cache"              
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL                ## Default null cache
        set <- function(mtx) {         ## SET function implementation
                x <<- mtx              ## Store the original matrix
                inverse <- NULL        ## Clear old cache
        }
        get <- function() x            ## GET command = get the original matrix
        
        ## Set the "cache" with supplied inversed matrix
        setinverse <- function(y) inverse <<- y
        
        ## Get inversed matrix from "cache"
        getinverse <- function() inverse
        
        ## Define available commands
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Inversion of a matrix can be time consuming if it is repeated multiple times.
## Instead of computing the inverse of a matrix every time, the cacheSolve function
## reuses previously cached matrix if one is available.
## When the function is called, it does 3 things:
## 1. Check for the availability of previously saved inversion of the matrix. If one is available, 
##    prints out a message, returns the cached matrix, and exits.
## 2. If cached inversed matrix is not available, compute the inverse of the matrix.
## 3. Stores a copy of inversed matrix in cache, returns the newly inversed matrix and exits.

## *** NOTE: The function works only if the matrix is invertible. Unpredictable operation may
##           result if non-invertible matrix is applied.
cacheSolve <- function(x, ...) {
        
        inverse <- x$getinverse()	## Get current cached inversed matrix 		
        
        ## Check for emptied cache.
        ## If cache is not empty, return cache content along with a message
        if (!is.null(inverse)) {
                message("getting data from cache")			
                return(inverse)
        }
        
        ## Here the cache is empty.
        ## 1. get the original matrix
        org <- x$get()
        
        ## 2. Compute matrix inversion
        inverse <- solve(org)
        
        ## 3. Save inveresed matrix into cache
        x$setinverse(inverse)
        
        ## Print the content of inversed matrix
        inverse
}
