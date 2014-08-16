

## A pair of functions that cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
			## Initialize the inverse
			i <- NULL			
		## Setting the matrix
		set <- function (matrix) {
			m <<- matrix
			i <<- NULL
		}
		## Getting the matrix
		get <- function() {
			m  ## Return the matrix
		}
		## Setting the inverse
		setInverse <- function(inverse) {
			i <<- inverse
		}
		## Getting the inverse
		getInverse <- function() {
			i ## Return the inverse
		}
		## Return List
		list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)		
}


## The following function computes the inverse of the special matrix returned by "makeCacheMatrix". "cachesolve"  will pull the inverse from the cache if the inverse has already been calculated. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        ## returning the already calculated inverse
        if( !is.null(m)) {
        	message("getting cached data")
        	return(m)
        }
        ## getting the matrix
        data <- x$get()
        ## calculating the inverse by multiplication
        m <- solve(data) %% data
        ## Setting the inverse to the object
        x$setInverse(m)
        ## Return the matrix
        m
}
