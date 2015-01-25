## Put comments here that give an overall description of what your
## functions do

## This function is used to create a special "matrix" object, that can cache its inverse
## which is calculated by the other function.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned
## by the above function, but it first checks whether the i is Null or not. 
## If i is not null i.e. inverse has already been calculated and the matrix has also
## not changed, then the cachesolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
  i <- x$getinverse() 
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get() 
  i <- solve(data, ...)
  x$setinverse(i) 
  i       ## Return a matrix that is the inverse of 'x'
}