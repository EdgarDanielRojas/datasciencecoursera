## Edgar Daniel Rojas Vazquez
## Functions that create a custom cache mechanism for the
## calculation of the inverse of a matrix, which is a costly
## calculation. 

## This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## Initially set cached inversed matrix to null
  inverse_matrix <- NULL
  ## Set function that sets the matrix to work with and resets cached inversed matrix
  set <- function(y) {
    x <<- y
    inverse_matrix <<- NULL
  }
  ## Get function that returns stored matrix
  get <- function() x
  ## setinverse function that stores the calculated inverse matrix to the cache
  setinverse <- function(inverse_var) inverse_matrix <<- inverse_var
  ## getinverse function that returns the cached inverse matrix
  getinverse <- function() inverse_matrix
  ## return a list of function so that the user can use them
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## Get the cached inverse matrix
  inverse_matrix <- x$getinverse()
  ## If a valid cached version is found, return the cached inverse matrix
  if(!is.null(inverse_matrix)) {
    message("getting cached data")
    return(inverse_matrix)
  }
  ## Calculate the inverse matrix and cache it
  data <- x$get()
  inverse_matrix <- solve(data, ...)
  x$setinverse(inverse_matrix)
  inverse_matrix
}
