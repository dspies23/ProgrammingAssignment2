## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# The first function, makeVector creates a special "vector", which is really a list containing a function to

# Set the value of the matrix
# Get the value of the matrix
# Set the value of the inverse matrix
# Get the value of the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
        matinv <- NULL
        set <- function(y) {
                x <<- y
                matinv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) matinv <<- inverse
        getinverse <- function() matinv
        list(
                set = set,
                get = get,
                setinverse = setinverse,
                getinverse = getinverse
        )
}


## Write a short comment describing this function
# The following function calculates the inverse of a matrix using a special "vector" from the above function.
# However, it first checks to see if the inverse has already been calculated.
# If so, it gets the inverse from the cache and skips the computation.
# Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache
# via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matinv <- x$getinverse()
        if(!is.null(matinv)) {
                message("getting cached data")
                return(matinv)
        }
        data <- x$get()
        matinv <- solve(data)
        x$setinverse(matinv)
        matinv
}
