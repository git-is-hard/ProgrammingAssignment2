## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
              cacheInv <- NULL
              saveInv <- function(x) cacheInv <<- x
              getMatrix <- function(x) x<<-x
              list(saveInv = saveInv, getMatrix = getMatrix)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  check <- x$saveInv()
  if (!is.null(cacheInv)){message("Getting cached data")
          return(check)
  }
  data <- x$getMatrix(x$getMatrix)
  matrixInv <- solve(data,...)
  x$saveInv(matrixInv)
  matrixInv
}
