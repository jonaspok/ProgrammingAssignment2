## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## gets or sets x matrix and its' inverse
  m <- NULL
  ## sets matrix x
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## gets matrix x
  get <- function() x
  ## sets inverse matrix of x
  setInverse <- function(mat) m <<-mat
  ## gets inverse matrix of x
  getInverse <- function() m
  ## returns a list of functions working with matrices
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  ## checks if an inverse matrix of x is cached
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  ## calculates new inverse matrix of x
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
