## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL    # Set NULL to m
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x    # Set x to get
  setsolve <- function(solve) m <<- solve     # use solve function to get cache 
  getsolve <- function() m      # give m to get solve
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolven)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cache solve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)      # set get solve to m, if NULL then get a message
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}