##  This Script uses the "<<-" operator to assign a value to objects in an
##  environment and for the object-value to be available in a different environment.
##  
##  Operationally, what this script does is generates a function to create a 
##  matrix object that can cache its inverse.  Then this script generates another
##  function that computes the inverse of the matrix.  But, if the inverse
##  has already been computed, it will be retreived from the cache instead of
##  being recalculated.



##  This function creates a special matrix object that can cache the matrix  
##  inverse.  The function uses the "<<-" operator to assign a value such that
##  the value is available in a different environment.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
       x <<- y
       m <<- NULL
  }
   
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  }
  
  

##  This function computes the inverse of the matrix using the "solve" function.
##  But it first uses an "if" control function to check if the inverse has
##  already been computed.  If it has been computed it is retreived from the cache.
##  The function returns the matrix inverse.

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
  
  }
