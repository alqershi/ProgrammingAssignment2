## There are two functions makeCacheMatrix which will make the matrix and acts as the cache manager, 
# and cacheSolve which returns the inverse from the cache if exisits 
# or calculate the inverse and store it in the cache

## makeCacheMatrix: It will compute and store the matrix inverse in a global enviroment
#
# Args:
#   x: The Square matrix which inverse should be calculated and stored.
#
# Returns:
#   a List of four functions setmatrix, getmatrix, setinvmatrix, getinvmatrix
#

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  setmatrix <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  getmatrix <- function() x
  
  setinvmatrix <- function(solve) m <<- solve
  
  getinvmatrix <- function() m
  
  list(
    setmatrix = setmatrix, getmatrix = getmatrix,
    setinvmatrix = setinvmatrix,
    getinvmatrix = getinvmatrix
  )
  
}


## cacheSolve: It will try to find the inverse from the cache first
#  if the inverse doesn't exisits, it will compute it and store in the cache.
#
# Args:
#   x: the list of functions from which the matrix is retrieved and to which the inverse is stored
#
# Returns:
#   the inverse of the matrix
#

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'.
  
  m <- x$getinvmatrix()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$getmatrix()
  m <- solve(data)
  x$setinvmatrix(m)
  
  m
}
