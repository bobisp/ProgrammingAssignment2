## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    set <- function(y) {
        inv <<- NULL
        x <<- y
    }
    get <- function() x
    setinv <- function(inver) inv <<- inver 
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}




## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if (!is.null(inv)) {
        message("Retrieved from cache:")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data,...)
    x$setinv(inv)
    inv
}
