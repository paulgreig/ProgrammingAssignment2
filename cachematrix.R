## Using functions to store a matrix and an inverse matrix
## 
## Usage Example:
##
##> source('cachematrix.R')
##> a <- makeCacheMatrix( matrix(c(4,2,7,6), nrow = 2, ncol = 2) );
##> a$get()
##     [,1] [,2]
##[1,]    4    7
##[2,]    2    6
##> a$getInverse()
##NULL

makeCacheMatrix <- function(x = matrix()) {

	i <- NULL
	# store the matrix
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    # return stored matrix
    get <- function() x
    # store an inverse matrix
    setInverse <- function(inv) i <<- inv 
    # return the stored inverse matrix
    getInverse <- function() i
    # return a list of the functions available
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}

## Using function to solve the inverse of a matrix or return a cached solution to avoid calculation
## 
## Usage Example:
##
##> cacheSolve(a)
##     [,1] [,2]
##[1,]  0.6 -0.7
##[2,] -0.2  0.4
##
## The first run performs the solve calculation of the matrix
## A second run of cacheSolve(a) will include message "getting cached data" indicating no calculation was performed 


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if(!is.null(i)) {
    	# uses the cached solution of inverse matrix
        message("getting cached data")
        return(i)
    }
    m <- x$get()
    i <- solve(m, ...)
    ## Cache the inverse matrix solution
    x$setInverse(i)
    ## Return the solved inverse matrix
    i
}
