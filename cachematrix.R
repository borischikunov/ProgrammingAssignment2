## This module provides means to inverse a matrix and cache the result
## for multiple future uses

## Create matrix with cache (also adapt an existing matrix)

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(value) {
		x <<- value
		inv <<- NULL
	}
	get <- function() x
	setInv <- function(newInv) inv <<- newInv
	getInv <- function() inv
	list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Inverse given matrix with cache and store the result of
## calculations into the cache. Return the result

cacheSolve <- function(x, ...) {
	inv <- x$getInv()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data)
	x$setInv(inv)
	inv
}
