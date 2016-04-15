## makeCacheMatrix: Function (x)
## Parameter List: 
##	x - a matrix of numbers is passed 
##		
##	Note: if non-numeric values passed in "solve" will choke with an informative message:
##  	Error in solve.default(data, ...) : 'a' must be a numeric matrix
##
##	Special Variables:
##	inverse - inverse of the matrix 
##	x		raw matrix	
##  	sub functions 
##  	"set" 		called to persist raw matrix cache and initialize inverse cache
##	"get"		get raw matrix
##  	"setInverse"	update inverse cache
##	"getInverse"	pass inverse cache

## Interact with the internal "special" cache

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}
	get <- function() x
	setInverse <- function(solve) inverse <<- solve
	getInverse <- function() inverse
	list(set = set, get = get,
		setInverse = setInverse,
		getInverse = getInverse)
}

## cacheSolve: Function (x)
## Parameter List: 
##	x - a matrix of numbers is passed 
##	... any other parms passed into other 
##		
##	Special Variables:
##	none:
##
##  	sub functions 
##  	"inverse"	use "getInverse" to pull cache value from special variables
##	if the cache contained data (value is NOT NULL)
##		thow a message "getting cached data"
##		return cached inverse value to caller
##	else
##		(cache is NULL)
##		get the raw matrix from cache
##		calculate the inverse via "solve" returning the value to inverse local variables
##		set the cached value of inverse to the local value via the setInverse function
##	end-if
##
## Caller interface to interact with the cache handler
cacheSolve <- function(x, ...) {
	inverse <- x$getInverse()
	if(!is.null(inverse)) {
		message("getting cached data")
		return(inverse)
	}
	data <- x$get()
	inverse <- solve(data, ...)
	x$setInverse(inverse)
}
