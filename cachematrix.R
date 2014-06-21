# makeCacheMatrix:This function creates a special "matrix" object 
# that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  iv <- NULL
  ## Set the value of the matrix
  set <- function(y) {
    x <<- y
    iv <<- NULL
  }
  ## Get the value of the matrix
  get <- function() x
  ## Set the value of the inverse
  setiv <- function(inverse) iv <<- inverse
  ## Get the value of the inverse
  getiv <- function() iv
  list(set = set, get = get, setiv = setiv, getiv = getiv)
}


# cacheSolve: This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above
cacheSolve <- function(x, ...) {
  iv <- x$getiv()
  ## Check if the inverse has already been calculated (and the matrix has not changed)
  if (!is.null(iv)) {
    message("getting cached data")
    ### retrieve the inverse from the cache
    return(iv)
  }
  ## Calculate the inverse 
  data <- x$get()
  iv <- solve(data, ...)  
  # Cache and return the inverse
  x$setiv(iv)
  iv
}
