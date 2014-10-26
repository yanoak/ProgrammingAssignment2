## Put comments here that give an overall description of what your
## functions do

## Creates a cache of a matrix and its inverse
##		function returns a list of functions to get and set
##		the cached value of the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {

	  ## initialize inverse to NULL
	  inverse <- NULL
	  
	  ## defines set() function, which will cache a new matrix 
	  set <- function(y) {
		x <<- y
		inverse <<- NULL
	  }
	  
	  ## defines get() function, which will retrieve stored matrix
	  get <- function() x
	  
	  ## defines set(), which will cache a new inverted matrix
	  setinverse <- function(newinverse) inverse <<- newinverse
	  
	  ## defines get(), which will retrieve the inverted matrix
	  getinverse <- function() inverse
	  
	  ## prepares the list of functions as a return value
	  list(set = set, get = get,
		   setinverse = setinverse,
		   getinverse = getinverse)
}


## If a cached answer for inverted matrix is available, 
##		cached answer is returned, otherwise computes inverse

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
			  
	  # Tries to get a cached inverted matrix.
	  #     If there is no cache, getinverse() returns NULL
	  #     and inverse is assigned NULL
	  inverse <- x$getinverse()
	  
	  # Checks to see if inverse is not NULL.
	  #     If inverse is not NULL, cached inverse is retrieved
	  #     and cacheSolve() returns the cached inverse
	  if(!is.null(inverse)) {
		message("getting cached data")
		return(inverse)
	  }
	  
	  # If inverse is NULL, the stored matrix is retrieved
	  data <- x$get()
	  
	  # Since inverse has not been cached, it is calculated
	  #     using solve() function
	  inverse <- solve(data)
	  
	  # Newly calculated inverse is cached
	  x$setinverse(inverse)
	  
	  # Inverse is returned by the function
	  inverse	
}
