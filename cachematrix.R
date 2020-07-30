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
	list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Calculates the inverse of the special "matrix" created by "makeCacheMatrix" function
## First checks if the inverse has been calculated
## If so, it gets the inverse from the cache and skips the computation
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setInv function.

cacheSolve <- function(x, ...) {

	inv <- x$getInv()
	if(!is.null(inv)){
		message("getting cached data")
	    return(inv)
	}
	data <- x$get()
	inv <- solve(data)
	x$setInv(inv)
	inv
}
