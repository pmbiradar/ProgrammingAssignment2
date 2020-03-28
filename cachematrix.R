#function to set the matrix and cache the inverse of the matrix

## Function to set and get the matrix , also to set and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverseOfMatrix <- function(solve) m <<- solve 
  getInverseOfMatrix <- function() m
  list(set = set, get = get,
       setInverseOfMatrix = setInverseOfMatrix,
       getInverseOfMatrix = getInverseOfMatrix)

}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        
  
  m <- x$getInverseOfMatrix() #checks for the inverse of the matrix x
  
  # below if loop will be executed - if there already exist inverse of the matrix x
  if(!is.null(m)) {
    message("getting inverse of matrix")
    return(m)
  }
  
  # compute  and return the inverse by using solve function
  data <- x$get()
  m <- solve(data)
  x$setInverseOfMatrix(m)
  m
  
}
