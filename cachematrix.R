
## This function will create a function with 4 methods
## set will store the original matrix
## get will return the original matrix
## setinv will set the inverse of the matrix, using the solve funtion
## getinv will return the inverted matrix
## note that getinv will return NULL until the cacheSolve funtion is applied to the matrix

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


## This function will store the inverse of the matrix that is created using the set command 
## in the makeCacheMatrix function
## the inout for this function is the output of makeCacheMatrix

cacheSolve <- function(x, ...) {
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

## Sample code used to test the functions 
## mymatrix <- matrix(c(-1, -2, 1, 1), 2,2)
## mynewmatrix <- makeCacheMatrix(mymatrix)
## cacheSolve(mynewmatrix)
## mynewmatrix$getinv()