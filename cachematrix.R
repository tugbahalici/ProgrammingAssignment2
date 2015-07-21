## makeCacheMatrix function creates a special matrix that caches its inverse as well.
## cacheSolve function solves the inverse of the special matrix. 

## The special matrix and its inverse is stored by x and i variables respectively. 
## Inverse is set to NULL by default. 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## If the inverse of the special matrix is cached, it is returned. 
## Otherwise, solve() function is called in order to solve for the inverse.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
