## Put comments here that give an overall description of what your
## functions do
 
## Write a short comment describing this function
##makeCacheMatrix creates a special "matrix" object that can cache its inverse:
 
#It initializes with an optional matrix.
#It provides methods to set/get the matrix and its inverse.
#The inverse starts as NULL and is only calculated when needed.
 
 
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
 
# Function to compute the inverse of the special matrix
## Write a short comment describing this function
#cacheSolve computes the inverse of the special matrix:
 
#It first checks if the inverse is already cached.
#If cached, it returns the cached inverse.
#If not, it calculates the inverse, caches it, and then returns it.
 
 
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
