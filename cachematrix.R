## Together, these two functions take a mtrix as an argument and then create 
## a "special object" that is a list of four new functions.  Then, functions in this
## list can be called to either retrieve the inverse of the martix argument from 
## a cached value, or, if no cached value exists, calculate the inverse and then
## store the result to the cache variable.

## makeCacheMatrix() takes a matrix as its only argument and creats a list of four new
## functions that 1) updates and stores a value for the matrix argument, 2) retrieve this 
## value, 3) stores a value for the to the a variable, inv, that will be used to cache
## the inverse of the matrix, 4) retrieves this value

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




## cacheSolve() calls on objects created by makeCacheMatrix() and, if there is no
## cached value (i.e., inv == NULL) it computes the inverse of the martix and stores it
## to inv

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
