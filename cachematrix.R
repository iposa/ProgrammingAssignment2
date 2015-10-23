
# Hance matrix inversion is a costly computation there may be some benefit
# in caching the inverse of a matrix rather than computing reapetedly 
# each time is neaded.
# 2 functions to cache the calculation of an inverse matrix were created

# The first function, `makeCacheMatrix` creates a special "matrix", which is
# really a list containing a function to
# 
# 1.  set the value of the matrix
# 2.  get the value of the matrix
# 3.  set the value of the inverse matrix
# 4.  get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


# The following function calculates the inverse matrix of the special "matrix"
# created with the above function. However, it first checks to see if the
# inverse matrix has already been calculated. If so, it `get`s the inverse matrix
# from the cache and skips the computation. Otherwise, it calculates the inverse matrix of
# the data and sets the value of the inverse matrix in the cache via the `setInverse`
# function.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  ## Return a matrix that is the inverse of 'x'
  inv
        
}

# #................
# # Example of use
# #................
# 
# # Function to crate a matrix with equal dimensions 
# CreateMatrixEqualDim <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
# 
# # Create matrix 6x6
# x<-CreateMatrixEqualDim(6)
# 
# # Cache matrix
# cacheMatrix<-makeCacheMatrix(x)
# 
# # Solve matrix
# cacheSolve(cacheMatrix)
# 
# # When solve again, retrieves imediatly result
# cacheSolve(cacheMatrix)
