## Assignment 2 - Inverse Matrix in cache.

## This function generates a matrix to be stored in cache.

makeCacheMatrix <- function(x = matrix()) {
        invM <- NULL
        set <- function(y){
                x <<- y
                invM <<- NULL
        }
        get <- function() x
        setinvM <- function(solve) invM <<- solve
        getinvM <- function() invM
        list(set=set, get=get,
             setinvM=setinvM,
             getinvM=getinvM)
}

## This function computes the inverse of the matrix and stores 
## it in cache, and if it is not the first time you run it, it
## is returned from the cache.

cacheSolve <- function(x, ...) {
        invM <- x$getinvM()
        if(!is.null(invM)) {
                message("Getting cached data")
                return(invM)
        }
        data <- x$get()
        invM <- solve(data, ...)
        x$setinvM(invM)
        print(invM)
}
## Return a matrix that is the inverse of 'x'