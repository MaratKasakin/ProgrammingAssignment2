## the function creates a special matrix object that can cache its inverse

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) { ## defines argument of the function
  inv_x <- NULL ## assign empty values to inverse matrix
  set <- function(y) { ## a new function sets new value of the matrix in parent environment
    x <<- y 
    inv_x <<- NULL ## values of inverse matrix are NULL for new matrix
  }
  get <- function() x ## returns values in parent environment
  setinv <- function(inv) inv_x <<- inv ## set value of inverse matrix in parent environment
  getinv <- function(inv_x) ## gets value of inverse matrix
    list(set = set, get = get, setinv = setinv, getinv = getinv)
} ## necessary step to deal with named arguments


## function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv_x <- x$getinv() ## takes inverse matrix values from the cache
  if(!is.null(inv_x) { ## if inverse matrix values are not NULL write message and return inverse matrix values from cache
    message("getting cached data")
    return(inv_x)
  }
  data <- x$get() 
  inv <- inv(data, ...) ## calculates inverse matrix values from data
  x$setinv(inv_x) ## assign inverse matrix values
  inv_x
}
