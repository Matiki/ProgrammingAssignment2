## Two functions that when working togther cache a matrix and its inverse.

## The first function creates a "matrix" object, 'x' that can cache its own inverse.
## In reality it returns a list containing functions to get/set the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        
        set <- function(y){
                x <<- y
                inverse <<- NULL
        }
        get <- function(){x}
        setinverse <- function(inverse_x){inverse <<- inverse_x}
        getinverse <- function(){inverse}
        
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## The second function calculates and returns the inverse of the matrix 'x'.
## If the inverse has already been solved and chached, it retrieves the inverse matrix
## from the cache.
cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)){
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
