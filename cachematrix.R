## GC 8/25/2016

## The functions in cachematrix work together to cache return the inverse of an invertible matrix. 
## If the matrix inverse as already been calculated and cached, the cache will be returned to avoid 
## using resources to calculate the matrix inverse again.

## The makeCacheMatrix function establishes a list of functions and environment relationships needed
## for the cacheSolve function to carry out the inverse calculation and caching

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  setinv <- function(solve) i <<- solve
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The cacheSolve function takes a list of objects that are the output of the makeCacheMatrix function
## and uses them to check if the inverse is cached, return it if it is, and calculate then cache if the
## inverse is not already present

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached inverse")
    return(i)
  }
  (data <- x$get())
  i <- solve(data, ...)
  x$setinv(i)
  i
}
