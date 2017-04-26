## Caching the Inverse of a Matrix
## We are using the assignment operator "<<-" to create an R object 
## instead of calling everything from global envi memory.
## cacheSolve returns Inverse if it is cached (if not, it still calculates and returns)

## The function below creates a special "matrix" object that can cache its inverse.
# Function returns a message if the Determinant of the matrix ==0
# Line No.10 is not required if we assume that the matrix given will be definitely invertible
makeCacheMatrix <- function(x = matrix()) {
  if (det(x) == 0){print("The matrix is not invertible.")}
  else {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  }
}



## The function below computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("Getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
## End of Code
