## Assignment 2 - Inverse Matrix in cache.

## This function (makeCacheMatrix) generates a matrix to be 
## stored in cache.

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

## This function (cacheSolve) computes the inverse of the matrix 
## and stores it in cache, and if the matrix is not changed and 
## if it is not the first time you run it, it is returned from 
## the cache (it shows the message "Getting cached data").

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
## Returns a matrix that is the inverse of 'x'

## * Example to run it: 
##     f <- matrix(rnorm(64,10,3),8,8)
##     a <- makeCacheMatrix(f)
##     cacheSolve(a)
