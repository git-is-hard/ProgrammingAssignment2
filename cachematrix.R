## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
              cacheInv <- NULL
              getMatrix <- function() x
              saveInv <- function(inv) cacheInv <<- inv
              checkCache <- function() cacheInv
              list(saveInv = saveInv, getMatrix = getMatrix, checkCache = checkCache)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  check <- x$checkCache()
  if (!is.null(check)){message("Getting cached data")
          return(check)
  }
  data <- x$getMatrix()
  matrixInv <- solve(data,...)
  x$saveInv(matrixInv)
  matrixInv
}
