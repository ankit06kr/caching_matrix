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
