## Pair of functions that manages cacheable matrices

## Creates a cacheable matrix
makeCacheMatrix <- function(x = matrix()) {
  inverse_matrix <- NULL
  set <- function(y) {
    x <<- y
    inverse_matrix <<- NULL
  }
  get <- function() x 
  setinverse <- function(inverse) inverse_matrix <<- inverse
  getinverse <- function() inverse_matrix
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## calculate the inverse of 'x' (using cache, if possible)
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse_matrix <- x$getinverse()
  if (!is.null(inverse_matrix)) {
    message("getting cached data")
    return(inverse_matrix)
  }
  data <- x$get()
  inverse_matrix <- solve(data)
  x$setinverse(inverse_matrix)
  inverse_matrix
}


