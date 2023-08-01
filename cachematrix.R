## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# The first function, makeCacheMatrix creates a special "matrix", which is really a matrix containing a function to
# 
# set the value of the matrix
# get the value of the matrix
# set the inverse of the matrix
# get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  set_inv <- function(solve) m <<- solve
  get_inv <- function() m
  list(set = set, get = get, 
       set_inv = set_inv, get_inv = get_inv)
}


## Write a short comment describing this function

# The following function calculates the inverse of the special "matrix" created 
# with the above function. However, it first checks to see if the inverse has 
# already been calculated. If so, it gets the inverse from the cache and skips 
# the computation. Otherwise, it calculates the inverse of the data and sets the 
# value of the inverse in the cache via the set_inv function.



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$get_inv()
  if(!is.null(m)) {
    message("getting the cached data")
    return(m)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$set_inv(inv)
  inv
}
