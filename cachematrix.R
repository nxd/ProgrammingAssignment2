## The functions below work together to calculate/cache/retrieve
## the inverse of an invertible 2-D matrix

## the below function returns a list of functions to get/set
## the matrix x, and to get/set the inverse of matrix x

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## The below function takes the list of functions from makeCacheMatrix
## and returns the inverse of the previously set matrix.
## if the inverse (m) has been previously cached, it is retrieved
## otherwise it is calculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
