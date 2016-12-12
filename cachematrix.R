  ## Create a function which starts with a null matrix argument that 
  ## store a matrix and its inverse

makeCacheMatrix <- function(Mat = matrix()) {
  inv <- matrix()
  set <- function(y) {
    Mat <<- y
    inv <<- matrix()
  }
  get <- function() Mat
  
  ## Calculates the inverse of non-singular matrix via the solve function
  
  setinverse <- function(solve) inv <<- solve
  
  ## Gets the inverse
  
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## A function to get the cache of inverse of a non-singular 
## matrix via the solve function matrix

cacheSolve <- function(Mat, ...) {
  inv <- Mat$getinverse()
  
  ## If the inverse already stored it retrieve the inverse
  
  if(!any(is.na(inv))) {
    message("getting cached data")
    return(inv)
  }
  
  ## If the inverse is not stored, first the invers is calculated 
  ## and then retrieved.
  
  data <- Mat$get()
  inv <- solve(data, ...)
  Mat$setinverse(inv)
  inv
}