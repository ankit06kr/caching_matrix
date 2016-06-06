# caching_matrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

caching_matrix <- function(x = matrix()) {
    invy <- NULL
    set <- function(y) {
        x <<- y
        invy <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) invy <<- inverse
    getInverse <- function() invy
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

#The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
    invy <- x$getinverse()
    if(!is.null(invy)) {
        message("getting cached data.")
        return(invy)
    }
    data <- x$get()
    invy <- solve(data)
    x$setinverse(invy)
    invy
}
