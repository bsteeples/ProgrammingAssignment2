## Functions which cache the inverse of a matrix to reduce computation time.

## First function to get and set the values in a list of a special "matrix" object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
}
get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Second function to calculate the inverse of the special "matrix" above- makeCacheMatrix, reusing the cashed result if available (assuming the inverse has already been calculated and the matrix has not changed).

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinverse(inv)
  inv
}
