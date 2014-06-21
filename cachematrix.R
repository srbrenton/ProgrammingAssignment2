# Below are two functions that are used to create a special object
# that stores a square matrix and caches its inverse.

# makeCacheMatrix()
# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

	m <- NULL
	
	set <- function( y ) {
				x <<- y
				m <<- NULL
			}

	# it's a good idea to enclose single statements in braces
	# the syntax checker in your brain will thank you
	get <- function( ) {
				x
			}

	# it's a good idea to enclose single statements in braces
	setinverse <- function( inverse ) {
					m <<- inverse
				}

	# it's a good idea to enclose single statements in braces
	getinverse <- function( ) {
					m
				}

	list( set = set, get = get, setinverse = setinverse, getinverse = getinverse )

}

# cacheSolve()
# This function computes the inverse of the special "matrix" returned by
# makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed),
# then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	m <- x$getinverse()

	if( !is.null(m) ) {
		message("getting cached data")
		return(m)
	}

	data <- x$get()

	m <- solve( data, ... )

	x$setinverse(m)

	m

}

