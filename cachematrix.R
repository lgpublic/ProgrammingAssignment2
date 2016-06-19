## Put comments here that give an overall description of what your
## functions do

## a strcture that wrap a matrix object
## and cache the inverse of it once computed and set

makeCacheMatrix <- function(x = matrix()) {
	## inversed matrix
	im <- NULL
	## set this obj with new matrix
	set <- function(y=matrix()) {
		x <<- y
		im <<- NULL
	}
	## get the matrix wrapped by this obj
	get <- function() x	
	## set inverse
	set_inverse <- function(i) im <<- i
	## get inverse
	get_inverse <- function() im
	
	list(set=set, get=get, set_inverse=set_inverse, get_inverse=get_inverse)

}


## function to get the inverse of a matrix
## get from cache if possible. otherwise compute it
## and cache it

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	im <- x$get_inverse()
	if (!is.null(im)) {
		message("got cached inverse matrix")
		return(im)
	}	
	data <- x$get()
	i <- solve(data, ...)
	x$set_inverse(i)
	i
}
