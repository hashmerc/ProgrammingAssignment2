## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inversion matrix rather than computing it repeatedly.
## Computing the inverse of a square matrix can be done with the solve function
## in R. For example, if X is a square invertible matrix then solve(X) returns 
## its inverse.
## Assumption: The matrix supplied for computing the inverse will always be a 
## square invertible matrix
## Example usage of the functions below:
## myTestMatrix <- makeCacheMatrix(matrix(c(1,2,3,4), 2, 2))
## cacheSolve(myTestMatrix)
## Result of the first execution:
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##
## Result of the subsequent executions for the same data show the caching enabled:
## cacheSolve(myTestMatrix)
## getting cached data
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## The following function creates a special "matrix" object that can cache its 
## inverse, which is really a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inverse <<- solve
        getinverse <- function() inverse
        list(set = set, get = get, setinverse = setinverse, 
             getinverse = getinverse)
}


## The following function calculates the inverse of the special "matrix" created
## with the above function. However, it first checks to see if the inverse has 
## already been calculated. If so, it gets the inverse from the cache and skips
## the computation. Otherwise, it calculates the inverse of the data and sets 
## the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse        
}
