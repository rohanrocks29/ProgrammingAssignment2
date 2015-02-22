# This code is written along the same lines of the example provided by 
# coursera. The first function calculates the inverse of the matrix and stores
# it in 'm' in the global environment.

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x

	# Calculate inverse of matrix using 'solve' function
	# And store it in m as cached value

        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## This function checks if a cached matrix exists. If it does then
# it returns the cached value and does not calculate the inverse

cacheSolve <- function(x = matrix(), ...) {

        # Check for cached value and return if existing
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }

	# If it doesnt exist, calculate inverse and store as cached
        data <- x$get()

	# Inverse of matrix
        m <- solve(data)
        x$setinverse(m)

	# Return the inverse
        m
}
