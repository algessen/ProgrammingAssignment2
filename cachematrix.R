## This file contains functions 'makeCacheMatrix' and 'cacheSolve'.
## Function 'makeCacheMatrix' creates from given matrix a special
## 'matrix' (actually, a list), that can cache value of 
## the matrix's inverse matrix. Function 'cacheSolve' takes
## as input the output of 'makeCacheMatrix' and returns matrix's 
## inverse matrix. If the inverse matrix had been already calculated,
## 'cacheSolve' takes it from cache, otherwise it calculate inverse
## matrix and saves it to cache.

## This function takes matrix as an argument and
## creates special 'matrix' object that can cache its invers. 
## Actually, it creates list, that contains four functions:
## 1. set - set the value of the matrix
## 2. get - get the value of the vector
## 3. setinverse - set the value of the inverse matrix
## 4. getinverse - get the value of the inverse matrix

makeCacheMatrix <- function(m = matrix()) {
    inverse <- NULL
    set <- function(x) {
        m <<- x
        inverse <<- NULL
    }
    get <- function() {
        m
    }
    setinverse <- function(inv) {
        inverse <<- inv
    }
    getinverse <- function() {
        inverse
    }
    list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special 'matrix
## returned by 'makeCacheMatrix' function. 
## If the inverse has already been calculated (and the matrix
## has not changed), the cached value is returned. Otherwise,
## the inverse matrix is calculated and saved to cache.

cacheSolve <- function(x, ...) {
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
