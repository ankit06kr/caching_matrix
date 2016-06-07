    #makecachematrix us a list containing functions to
        ##              1. set the matrix
        ##              2. get the matrix
        ##              3. set the inverse
        ##              4. get the inverse
        ##         this list is used as the input to cacheSolve()
        
makecachematrix <- function(x = matrix()) {
    invy <- NULL
    set <- function(y) {
        x <<- y
        invy <<- NULL
        # use `<<-` to assign a value to an object in an environment 
        # different from the current environment. 
    }
    get <- function() x
    setInverse <- function(inverse) invy <<- inverse
    getInverse <- function() invy
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

#cacheSolve

cacheSolve <- function(x, ...) {
## @x: output of makeCacheMatrix()
## return: inverse of the original matrix input to makecachematrix()

    invy <- x$getinverse()
    # if the inverse has already been calculated
    if(!is.null(invy)) {
    # get it from the cache and skips the computation. 
        message("getting cached data.")
        return(invy)
    }
    # otherwise, calculates the inverse 
    data <- x$get()
    invy <- solve(data)
    # sets the value of the inverse in the cache via the setinverse function.
    x$setinverse(invy)
    invy
}
