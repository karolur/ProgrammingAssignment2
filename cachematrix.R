## Functions that cache the inverse of a matrix

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
	I <- NULL
	set <- function(y) {
		x <<- y
		I <<- NULL
	}
  	get <- function() x
  	setI <- function(Inverse) I <<- Inverse
  	getI <- function() I
  	list(set = set, get = get,
   	    setI = setI,
   	    getI = getI)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	  I <- x$getI()
	  if(!is.null(I)) {
	  	message("getting cached data")
    	return(I)
    }
  	data <- x$get()
  	I <- solve(data, ...)
  	x$setI(I)
  	I
}
