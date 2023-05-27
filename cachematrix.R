# Create a special matrix object that can cache its inverse
makeCacheMatrix <- function(x) {
  # Initialize the cache to NULL
  cache <- NULL
  
  # Function to set the matrix data and cache
  set <- function(y) {
    x <<- y  # Store the matrix in the parent environment
    cache <<- NULL  # Reset the cache
  }
  
  # Function to retrieve the matrix data
  get <- function() {
    x  # Return the matrix
  }
  
  # Function to compute and cache the inverse of the matrix
  cacheInverse <- function() {
    if (!is.null(cache)) {
      message("Getting cached inverse")
      return(cache)  # Return the cached inverse if available
    }
    
    message("Calculating inverse")
    inv <- solve(x)  # Compute the inverse using solve()
    cache <<- inv  # Cache the computed inverse
    inv  # Return the inverse
  }
  
  # Return a list of the defined functions
  list(set = set, get = get, cacheInverse = cacheInverse)
}

# Compute the inverse of the cached matrix
cacheSolve <- function(cacheMatrix) {
  cacheMatrix$cacheInverse()  # Call the cacheInverse function of the cacheMatrix
}
