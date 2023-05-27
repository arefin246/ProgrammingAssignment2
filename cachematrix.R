# makeCacheMatrix function
makeCacheMatrix <- function(x) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() {
    x
  }
  
  cacheSolve <- function() {
    if (!is.null(inv)) {
      message("Getting cached inverse")
      return(inv)
    }
    
    message("Calculating inverse")
    inv <- solve(x)
    cacheSolve()
  }
  
  list(set = set, get = get, cacheSolve = cacheSolve)
}

# cacheSolve function
cacheSolve <- function(cacheMatrix) {
  cacheMatrix$cacheSolve()
}

# Test your functions
# Create a matrix
mat <- matrix(c(2, 4, 1, 3), nrow = 2)

# Create a cacheMatrix object
cacheMat <- makeCacheMatrix(mat)

# Retrieve the matrix
cacheMat$get()
# Output:
#      [,1] [,2]
# [1,]    2    1
# [2,]    4    3

# Compute the inverse and cache it
cacheSolve(cacheMat)
# Output:
# Calculating inverse
# Getting cached inverse
#           [,1] [,2]
# [1,] -1.5   0.5
# [2,]  2.0  -1.0

# Retrieve the cached inverse
cacheSolve(cacheMat)
# Output:
# Getting cached inverse
#           [,1] [,2]
# [1,] -1.5   0.5
# [2,]  2.0  -1.0
