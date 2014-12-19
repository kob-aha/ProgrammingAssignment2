## The following functions calculates the inverse of a matrix.
## Matrix iversion is usually costly so we will create a caching mechanism to store the inverse matrix and 
## recalculate it only when the matrix changes. 

## The following function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(mat = matrix()) {
  inversemat <- NULL
  setmat <- function(newmat) {
    mat <<- newmat
    inversemat <<- NULL
  }
  getmat <- function() mat
  setimat <- function(imat) inversemat <<- imat
  getimat <- function() inversemat
  list(setmat = setmat, 
       getmat = getmat,
       setimat = setimat,
       getimat = getimat)
}


## The following function computes the inverse of a matrix returned by "makeCacheMatrix". 
## It uses "makeCacheMatrix" caching ability and make sure to calculate the inverse matrix only once.
cacheSolve <- function(mat, ...) {
  imat <- mat$getimat()
  if(!is.null(imat)) {
    message("getting cached data")
    return(imat)
  }
  datamat <- mat$getmat()
  imat <- solve(datamat, ...)
  mat$setimat(imat)
  imat      
}
