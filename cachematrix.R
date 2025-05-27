cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
# This function creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # This will hold the cached inverse
  
  # Function to set a new matrix and reset the cached inverse
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Function to get the current matrix
  get <- function() x
  
  # Function to set (cache) the inverse of the matrix
  setinverse <- function(inverse) inv <<- inverse
  
  # Function to get the cached inverse (if it exists)
  getinverse <- function() inv
  
  # Return a list of the above functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# This function computes the inverse of the matrix created by makeCacheMatrix
cacheSolve <- function(x, ...) {
  # Check if the inverse has already been calculated
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached inverse")  # Inform user that cached data is used
    return(inv)
  }
  
  # If not cached, compute the inverse
  mat <- x$get()
  inv <- solve(mat, ...)  # 'solve' calculates the inverse of a matrix
  
  # Cache the inverse for future use
  x$setinverse(inv)
  
  # Return the inverse
  inv
}

