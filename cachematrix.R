## Programming Assignment 2
## Cache the inverse of a matrix

## Function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	a <- NULL
	set <- function(y) {
		x <<- y
		a <<- NULL
	}
	get <- function() x
	setinvs <- function(solve) a <<- solve
	getinvs <- function() a
	list(set = set, get = get, setinvs = setinvs, getinvs = getinvs)
}


## Function computes the inverse of the matrix from the makeCacheMatrix function
## If the inverse has already been calculated, gets the inverse from the cache
## Otherwise, calculates the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	a <- x$getinvs()
	if(!is.null(a)) {
		message("getting cached data")
		return(a)
	}
	data <- x$get()
	a <- solve(data, ...)
	x$setinvs(a)
	a
}
