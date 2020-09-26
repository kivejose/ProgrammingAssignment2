## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    
    #creating a inverse value placeholder
    inverse <- NULL
    
    set <- function(g) {
        x <<- g
        inverse <<- NULL
    }
    # getting the original matrix
    get <- function() x
    # setting inverse value
    setinverse <- function(invs) inverse <<- invs
    # getting inverse value
    getinverse <- function() inverse
    
    # Returns a list of the 4 functions, this list is the special "matrix"
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Returns a matrix that is the inverse of 'x'
    
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}