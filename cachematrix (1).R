## Put comments here that give an overall description of what your
## functions do
#The two functions: makeCacheMatrix and cacheSolve provide a way to cache the inverse of a matrix



## Write a short comment describing this function
#The makeCacheMatrix function creates a special "matrix" object that can cache its inverse, 
#which contains functions to set and get the matrix, as well as to set and get the cached inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## The cacheSolve function computes the inverse of the "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated and cached, it retrieves the cached value.
## Otherwise, it calculates the inverse, stores it in the cache, and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
