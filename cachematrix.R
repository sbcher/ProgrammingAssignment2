## The makeCacheMatrix and cacheSolve functions cache the inverse of a matrix. 
  ## This example of caching an inverted matrix illustrates the concepts of 
  ## scoping and creating an S3 object in R.

## makeCacheMatrix function creates an R object that stores a matrix and its inverse.
    ## makeCacheMatrix function builds a set of functions and 
    ## returns the functions within a list to the parent environment.
          ## makeCacheMatrix returns an object that contains four functions: 
          ## set(), get(), setInverse(), and getInverse(). 
                ## It also includes the two data objects, x and m.

makeCacheMatrix <- function(x = matrix()) {  
  ## Note: the matrix supplied should be a square invertible matrix.
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The casheSolve function calculates the inverse of the special "matrix" 
## created with the makeCacheMatrix function.If the inverse has already 
## been calculated (and the matrix has not changed), then the cacheSolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
  
