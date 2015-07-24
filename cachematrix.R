## These two functions should allow to store a square matrix and its inversion in a closure
## to access and change the matrix and its inversion, a list with accessor functions is used
## makeCacheMatrix creates the list of accessor functions and closure
## cacheSolve calculates the inverse and stores it in the closure 

## makeCacheMatrix: takes a square matrix, creates a closure containing it and potentially the inversion,
## and returns a list with accessor functions
## the function does no checking, it expects to receive only invertible square matrices
## the functions are set, get, setinv and getinv, for setting and retrieving the matrix resp. its inverse
## default matrix: empty matrix()
# usage example:
# a <-  makeCacheMatrix(matrix(c(1,2,-2,4),nrow=2))
# a$get()
# a$set(matrix(rnorm(9,10,5),nrow=3))
makeCacheMatrix <- function(x = matrix()) {
  # set the inverted matrix to NULL in the function environment
  xi <- NULL
  # set: allows to change the matrix held and resets the cached inverted matrix to NULL
  set <- function (y) {
    # set matrix x in function env. to new value y
    x <<- y
    # reset cached inverted matrix xi to NULL
    xi <<- NULL
  }
  # get: get the currently set matrix
  get <- function() x
  # setinv: overwrite the currently held inverted matrix
  setinv <- function(newxi) xi <<- newxi
  # getinv: return currently held inverted matrix xi
  getinv <- function() xi
  # return list of accessor functions
  return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}


## cacheSolve: this function return the inverse of the matrix held in the closure created by makeCacheMatrix
## if the inverse has not yet been calculated - or is not cache - it calculates and caches it
## it receives a list as created by makeCacheMatrix and returns the inverted matrix
## in case the matrix is not invertible, it returns 'NA' and does not set any value
## additional arguments are forwarded to 'solve'
# usage: cacheSolve(a)
cacheSolve <- function(x, ...) {
  # retrieve variable holding inverted matrix
  xi <- x$getinv()
  # if inverted matrix cached return that matrix
  if (class(xi) == "matrix"){
    message("returning cached inverted matrix")
    return(xi)
  }
  # check if matrix is invertible, if not return 'NA'
  if (det(a$get()) == 0) {
    message("matrix not invertible, no value set for caching")
    return(NA)
  }
  # if xi holds NULL or something not a matrix, calculate new inversion
  xi <- solve(x$get(), ...)
  # store the result in closure
  x$setinv(xi)
  # return the newly calculated matrix
  return(xi)
}
