## Put comments here that give an overall description of what your
## functions do
## Usage:
## 1. Create a matrix: B = matrix(c(2,4,6,8,1,3,5,7,9), nrow=3, ncol=3)
## 2. build the special matrix vector: v = makeCacheMatrix(B)
## 3. Compute the inverse: cacheSolve(v)
## Note: Since the inverse is stored in the context of the vector returned by
## makeCacheMatrix(), it is not possible to change the matrix between makeCacheMatrix()
## and cacheSolve(), so we do not neeed to check if the matrix has changed in cacheSolve()


## Write a short comment describing this function
## build the "special matrix". 
## input is the original matrix whose inverse needs to be cached.
## returns a vector representing the matrix and its inverse that can be called 
## by cacheSolve()
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    get <- function() x
    
    set_inv <- function(inv) {
      m <<- inv
    }
    
    get_inv <- function() m
    
    list(get = get, set_inv = set_inv, get_inv = get_inv)
}


## Write a short comment describing this function
## returns the inverse of a matrix passed to makeCacheMatrix()
## returns the inverse from the cache, if available else the inverse is 
## computed and cached before returning to the caller. 
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$get_inv()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data)
    x$set_inv(inv)
    inv
}
