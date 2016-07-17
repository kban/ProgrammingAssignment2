## input: x - a square invertible matrix (For this assignment, assume that the matrix supplied is always invertible.)
## return: a list with following functions:
##              set - sets the matrix
##              get - gets the matrix
##              setSolve - set the inverse
##              getSolve - get the inverse
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) s <<- solve
  getSolve <- function() s
  
  list(set=set, get=get, setSolve=setSolve, getSolve=getSolve)
}

## input: x - extended matrix (result of makeCacheMatrix() method)
## return: a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  s <- x$getSolve()
  
  ## if inverse is already exists then returning it
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  
  data = x$get()
  s = solve(data, ...)
  x$setSolve(s)
  s
}

