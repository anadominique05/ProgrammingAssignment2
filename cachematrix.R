## These functions cache the inverse of a matrix rather than compute it repeatedly

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	InverseVal <- NULL
	set <- function(y) {
		x <<- y
		InverseVal <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) InverseVal <<- inverse
	getInverse <- function() InverseVal
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the matrix returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then this function should retrieve the mean from the cache

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
	InverseVal <- x$getInverse()

		if(!is.null(InverseVal)) {
			message("getting cached data")
			return(InverseVal)
		}
		else if(class(try(solve(InverseVal), silent = T)) != "Matrix") {
			message("Data not inversible matrix")
			return(InverseVal)
		}
	datate <- x$get()
	InverseVal <- solve(data, ...)
	x$setInverse(InverseVal)
	InverseVal
}
