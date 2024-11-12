## Put comments here that give an overall description of what your
## functions do
## These functions create a special matrix object that can cache its inverse,
## allowing the inverse to be computed once and retrieved from the cache for
## subsequent requests.
## Write a short comment describing this function
## This function creates a special matrix object that can cache its inverse.
## It contains functions to set and get the matrix, and to set and get the inverse.
makeCacheMatrix <- function(x = matrix()) {
  cachedInverse <- NULL
  set <- function(y) {
    x <<- y
    cachedInverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) cachedInverse <<- inverse
  getInverse <- function() cachedInverse
  list(set = set, get = get,setInverse = setInverse,getInverse = getInverse)
}


## Write a short comment describing this function
## This function computes the inverse of the special matrix returned by makeCacheMatrix function.
## If the inverse has already been calculated (and the matrix has not changed),
#then it retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invFunc <- x$getInverse()
  if(!is.null(invFunc)) {
    message("getting cached data")
    return(invFunc)
  }
  data <- x$get()
  invFunc <- solve(data, ...)
  x$setInverse(invFunc)
  invFunc
}




