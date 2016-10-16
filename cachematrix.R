#Programming Assignment 2

#Description: Calculating repeated functions, for example, those in a loop may be costly
# thus by cache the results we can pull them again.

# makeCacheMatrix - creates a matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL}
  
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



# CacheSolve - computes the inverse of the matrix returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
         message("getting cached data.")
    return(inv)
    }
    data <- x$get()
    inv <- solve(data,...)
    x$setinverse(inv)
    inv
}
