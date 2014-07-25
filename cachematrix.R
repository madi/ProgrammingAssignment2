
############################################################################
#
# MODULE:      cachematrix.R
# AUTHOR:      Margherita Di Leo
# PURPOSE:     Return the inverse of a square matrix. 
#              Script developed as assignment of the R-programming course 
#              (Coursera) with the aim of exploiting the lexical scoping for 
#              optimizing potentially time consuming computations. This script  
#              is caching the inverse of the matrix.
#
#              This program is free software under the GNU General Public
#              License (>=v3.0) and comes with ABSOLUTELY NO WARRANTY.
#
#############################################################################


## Create a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Compute the inverse of the matrix. If the inverse has already been calculated,
## and the matrix has not changed, then it retrieves the inverse from the cache

cacheSolve <- function(x = matrix(), ...) {
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



