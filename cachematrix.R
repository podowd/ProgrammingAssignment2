## Functions for calculating and caching the inverse of a Matrix - once the inverse is cached it 
## can be continuously accessed without having to be recalculated (unless the original matrix chnages!)

## This function creates a custom matrix object that can also store the matrix's corresponding inverse matrix 
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(matrix) {
    x <<- matrix;
    inverse <<- NULL;
  }
  get <- function() return(x);
  setInverse <- function(invMatrix) inverse <<- invMatrix;
  getInverse <- function() return(inverse);
  return(list(set = set, get = get, setInverse = setInverse, getInverse = getInverse))  
}

## This function returns the inverse of a matrix - it first checks if the inverse has already been computed and 
## if yes, just returns it. 
## if not, it first calculates the inverse matrix, 
##         - then caches it by calling the setInverse function of the special matrix object (which is returned by the makeCacheMatrix function above)
##         - and finally returns the inverse matrix. 
cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("Using cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data,...) #Calculates the inverse matriz 
  x$setInverse(inverse)
  return(inverse)
}