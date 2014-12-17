## Put comments here that give an overall description of what your
## functions do

## [Echo]A special object which contains Matrix and its Inverse
## and functions that can set or get the value of Matrix 
## and its Inverse

makeCacheMatrix <- function(x = matrix()) {
	inverseM <- NULL
	set <- function(y) {
		x <<- y
		inverseM <<-NULL
	}
	get <- function() x
	setInverseM <- function(invM) inverseM <<- invM
	getInverseM <- function() inverseM
	list(set = set, get = get, 
			setInverseM = setInverseM,
			getInverseM = getInverseM)
}


## [Echo] Get the Inverse Matrix of x, if x's inverse has been
## calculated and cached, then return its inverse.
## Otherwise, do calculation and return the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invM <- x$getInverseM()
        if(!is.null(invM)){
        	message("getting cached inverse matrix")
        	return(invM)
        }
        message("caculating inverse matrix")
        mat <- x$get()
        invM <- solve(mat)
        x$setInverseM(invM)
        invM
}
