## The 'makeCacheMatrix' function creates a special "matrix" object
## that can cache its inverse. Its output is used as the input to the
## solveCache command listed below.

makeCacheMatrix <- function(x = numeric()) {
    #Sets the value of 'm' equal to NULL
    m <- NULL
    #Creates a 'set' variable to store a new value (if there is one)
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    #Creates a 'get' variable to retrieve the previous value cached in
    #memory
    get <- function() x
    #Creates a 'setinverse' variable to hold the value of the inverse
    #of the matrix you've input
    setinverse <- function(solve) m <<- solve
    #Creates a 'getinverse' variable to retrieve the previous inverse
    #cached in memory
    getinverse <- function() m
    #Creates an output in list format as an input for the cacheSolve
    #command
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The 'cacheSolve' function returns the inverse of the matrix initially
## input into the makeCacheMatrix command above. If the inverse has
## already been calculated (and the matrix has not changed), then
## the 'cacheSolve' should retrieve the inverse from the cache.
## Otherwise, it calculates the inverse and sets the cached value for
## the inverse equal to the newly calculated matrix

cacheSolve <- function(x, ...) {
    #set 'm' equal to whatever is cached in memory for the inverse of
    #the matrix you called in the makeCacheMatrix command
    m <- x$getinverse()
    #if there is a value in memory for it, great, pull that in
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    #otherwise, get the matrix
    data <- x$get()
    #solve for the inverse of that matrix
    m <- solve(data, ...)
    #cache your new inverted matrix into memory
    x$setinverse(m)
    #and return the value
    m
}
