## The pair of functions makeCacheMatrix and cacheSolve work together to cache 
## the inverse of a matrix avoiding the recalculation of an inverse matrix. 
## Note: the matrix supplied is a square invertible matrix.

# makeCacheMatrix function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(InverseMatrix) m <<- InverseMatrix
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

# The casheSolve function calculates the inverse of the special "matrix" 
# created with the makeCacheMatrix function. It checks if the inverse 
# has already been calculated (and the matrix has not changed). If yes, 
# the cacheSolve retrieves the inverse from the cache. If not, it will 
# calculate and cache it. 

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

# a function to test the cached inversion code above
# adapted from Chris Roeder

my_test <- function() {
  
  matrix_1 = matrix(c(2,4,3,1,5,7,8,9,6), nrow=3, ncol=3, byrow=TRUE)
  
  cache_matrix = makeCacheMatrix(matrix_1)
  
  message("starting matrix")
  
  print(matrix_1)
  
  message("solved matrix")
  
  print(solve(matrix_1))
  
  message("---------------")
  
  message("using cacheSolve the first time")
  
  print(cacheSolve(cache_matrix))
  
  message("using cacheSolve the second time")
  
  print(cacheSolve(cache_matrix))
  
  
  
  message("\n switching matrix ")
  
  matrix_2 = matrix(c(1,0,0,0,1,0,0,0,1), nrow=3, ncol=3, byrow=TRUE)
  
  cache_matrix = makeCacheMatrix(matrix_2)
  
  print(matrix_2)
  
  message("solved matrix")
  
  print(solve(matrix_2))
  
  message("---------------")
  
  message("using cacheSolve the first time")
  
  print(cacheSolve(cache_matrix))
  
  message("using cacheSolve the second time")
  
  print(cacheSolve(cache_matrix))
  
}



my_test()

