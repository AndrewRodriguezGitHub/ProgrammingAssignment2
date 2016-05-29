## Cache potentially time-consuming computations:
## Take advantage of the scoping rules of the R language and 
## how they can be manipulated to preserve state inside of an R object.
## This work is to write a pair of functions, one is used to set the origianl matrix, and the other 
## checkes the cache for its inverse, and returns cached inverse or computes and sets inverse, as needed.

## call this function first: it returns the list object to set and get the origianl matrix and the inverse of the original matrix, and to pass to cacheSolve 
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(passedInverse) m <<- passedInverse
  getInverse <- function() m
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## call this function second, passing the object retuned by makeCacheMatrix()
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)){
    message("cachematrix.R: Getting cached inverse of matrix")
    return(m)
  }
  message("cachematrix.R: No cached inverse; solving and setting the inverse")
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setInverse(m)
  m
}
