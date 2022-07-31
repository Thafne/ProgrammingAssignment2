## The makeCacheMix consists of set, get, setinv and getinv
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  ##initializing inverse as NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL  
  }
  get <- function() x         ## function to get matrix x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv         ##function to obtain inverse of the matrix 
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}
##The cacheSolve is used to get the cache data
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached result")
    return(inv)   ##Returns inverse value
  }
  data <- x$get()
  inv <- solve(data, ...)  ## This calculete inverse value
  x$setinv(inv)
  inv       ##Return a matrix that is the inverse of 'x'
}

## For chechinkg the program:
m <- matrix(rnorm(16),4,4)
m1 <- makeCacheMatrix(m)
cacheSolve(m1)
