## The below functions create a matrix, calculate and cache its ##inverse, and return its inverse from cache

## makeCacheMatrix is a function that creates a matrix object  
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
}
		get <- function() x
		set.inverse <- function(inv.ma) inverse <<- inv.ma
		get.inverse <- function() inverse
		list (set=set, get=get, 
			  set.inverse = set.inverse,
			  get.inverse=get.inverse)
			  }
			  


## cacheSolve computes the inverse of the matrix returned 
##by makeCacheMatrix. If the inverse has already been calculated, 
## cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        inverse <- x$get.inverse()
        if(!is.null(inverse)) {
        	message("getting cached data")
        	return(inverse)
        }
        my.matrix <- x$get()
        inverse <- solve(my.matrix, ...)
        x$set.inverse(inverse)
        inverse
}

