## First create the special matrix with the get and set methods
## Then calculate the inverse if the matrix has changed

##  Create a special “matrix” that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Calculate the inverse of the special “matrix” from the function makeCacheMatrix.
## First, it checks if the inverse has already been calculated.
## If it has been calculated, it gets the inverse from the cache and skips the computation.
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}