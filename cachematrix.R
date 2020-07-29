## Put comments here that give an overall description of what your
## functions do

## creates a special "Matrix" that cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setInv <- function(solveMatrix) inv <<- solveMatrix
	getInv <- function() inv
	list(set = set, get = get, setmean = setInv, getmean = getInv)
}


## calculates the inverse of the special "matrix" created with the above function

cacheSolve <- function(x, ...) {
	 ## Return a matrix that is the inverse of 'x'

	inv <- x$getInverse()
	if(!is.null(inv)){
		message("getting cached data")
	    return(inv)
	}
	data <- x$get()
	inv <- solve(data)
	x$setInverse(inv)
	inv
}
