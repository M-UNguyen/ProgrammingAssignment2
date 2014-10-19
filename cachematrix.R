## This file contains two functions:
## 1. makeCacheMatrix, which creates a matrix object that can cache its inverse.
## 2. cacheSolve, which computes the inverse of the special matrix object created by makeCacheMatrix.
## It is assumed that the matrix is always invertible.


## makeCacheMatrix creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inverse_of_x <- NULL
	set <- function(y) {
		x <<- y
		inverse_of_x <<- NULL	## re-initialize inverse in cache when matrix changes
	}
	get <- function() x
	setinverse <- function(solve) inverse_of_x <<- solve
	getinverse <- function() inverse_of_x
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}

## cacheSolve computes the inverse of the matrix object returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve
## retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
	inverse_of_x <- x$getinverse()
	if ( !is.null(inverse_of_x) ) {		## inverse has been calculated, return cached data
		message("getting cached data")
		return(inverse_of_x)
	}

	## else solve for inverse, cache, and return newly calculated data
	message("solving for inverse")
	data <- x$get()
	inverse_of_x <- solve(data, ...)
	x$setinverse(inverse_of_x)
	inverse_of_x
}
