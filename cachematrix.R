## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix receive a matrix, then save it to getMatrix.
## saveInv receive inverse matrix then save it to global memory as cacheInv
## checkCache return the value in cacheInv to check if there already have inverse
## result or not.
makeCacheMatrix <- function(x = matrix()) {
              cacheInv <- NULL
              getMatrix <- function() x
              saveInv <- function(inv) cacheInv <<- inv
              checkCache <- function() cacheInv
              list(saveInv = saveInv, getMatrix = getMatrix, checkCache = checkCache)
}


## Write a short comment describing this function
## first it take the result of function above then check if there are saved result
## or not
##else, below math  take the matrix by lexical scoping and return the result
## and use saveInv to save result in cacheInv
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
