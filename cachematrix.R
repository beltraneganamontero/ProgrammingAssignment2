
# Programming Assignment 2: Lexical Scoping
# Caching the inverse of a matrix


# Creates a matrix object that can store its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  inverse <- NULL
  
  # Updates the matrix and clears the previous inverse.
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  # Returns the current matrix.
  get <- function() {
    x
  }
  
  # Stores the calculated inverse.
  setsolve <- function(value) {
    inverse <<- value
  }
  
  # Returns the cached inverse.
  getsolve <- function() {
    inverse
  }
  
  list(
    set = set,
    get = get,
    setsolve = setsolve,
    getsolve = getsolve
  )
}


# Returns the inverse, computing it only when necessary.
cacheSolve <- function(x, ...) {
  
  inverse <- x$getsolve()
  
  # Use the cached result if it already exists.
  if (!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  # Calculate and store the inverse.
  data <- x$get()
  inverse <- solve(data, ...)
  x$setsolve(inverse)
  
  return(inverse)
}

