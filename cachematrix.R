##  There are two functions that are used to create a special object that stores 
## a matrixes and cache's its inverse.

## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to
## 
## set the value of the matrix (set)
## get the value of the matrix (get)
## set the value of the inverse matrix (setInverse)
## get the value of the inverse matrix (getInverse)


makeCacheMatrix <- function(x = matrix()) {
  # variable that contain cashed value of inverse matrix
  minverse <- NULL
  
  setMatrix <- function(y) {
    x <<- y
    minverse <<- NULL
  }
  
  getMatrix <- function() x
  setInverse <- function(inverse) minverse <<- inverse
  getInverse <- function() minverse
  list(set = setMatrix, get = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}




## The following function calculates the inverse matrix of the special "matrix" 
## created with makeCacheMatrix function. 
## It first checks to see if the reverse matrix has already been calculated.
## If so, it gets the inverse matrix from the cache and skips the computation.
## Otherwise, it calculates the inverse matrix of the matrix and sets 
## the value of the inverse matrix in the cache via the setInverse function.

## The function returns a matrix that is the inverse matrix of 'x'

cacheSolve <- function(x, ...) {
  
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  mat <- x$get()
  # calculate the inverse matrix
  matI <- solve(mat, ...)
  x$setInverse(m)
  matI
}
