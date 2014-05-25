## Functions calculate an inverse to the matrix and cache it, so that 
## the caculation doesn't need to be repeated. Function assumeds input
## matrix is square

## Creates a special vector with functions to set and get the matrix and its solution

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y){
		x <<- y	
		i <<- NULL
	}

	get <- function() x
	setinv <- function(solve) i <<- solve(x)
	getinv <- function() i
	return (list(set = set, get = get, setinv = setinv, getinv = getinv))
	
}


 ## Return a matrix that is the inverse of 'x', unless an inversed is already chached

cacheSolve <- function(x, ...) {
  m <- x$getinv()
	if(!is.null(m)) {
		messge("getting cached data")	
		return(m)
	        }
	        
	data <- x$get()
	m <- solve(data, ...)
	x$setinv(m)
	m
	
}
 

