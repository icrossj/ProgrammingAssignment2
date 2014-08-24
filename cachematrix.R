# There are two functions in this R file.

## makeCacheMatrix will define an object with 4 functions
# (1) get (2) set (3) getinverse (4) setinverse
 
# (1) get will return the matrix stored in the object
# (2) set will set a new matrix, and remove any cached inverse
# (3) setinverse will set an inverse of the matrix
#		this may or may not be correct
# (4) getinverse will call the stored inverse (if any)

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(mean) m <<- mean
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve

# for use with an object created by makeCacheMatrix

# This will first check to see if the matrix object has an inverse matrix
# stored. If yes, then it will return that stored inverted matrix and
# print it to screen.

# If not, it will calculate a new inverse matrix, print it to screen,
# and set it as the cache for the matrix object.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        ## Return a matrix that is the inverse of 'x'
}


