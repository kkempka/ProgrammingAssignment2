## The makeCacheMatrix and cacheSolve functions provide the ability to cache
## a matrix and its inverse for future use

## Creates an object that allows you to store and pull a matrix and it's inverse

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
      	x <<- y
            m <<- NULL
      }
      get <- function() x
      setinv <- function(inv) m <<- inv
      getinv <- function() m
      list(set = set, get = get,
      	setinv = setinv,
            getinv = getinv)
}


## Checks if inverse to matrix has already been found, if not will calculate it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getinv()
	if(!is.null(m)) {
      	message("getting cached data")
      	return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinv(m)
      m
}
