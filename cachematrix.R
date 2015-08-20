## Put comments here that give an overall description of what your
## functions do

## this creates a list of four functions operating upon an internal matrix x  

makeCacheMatrix <- function(x = matrix()) { 
  inverted <- NULL
## calling this function opens a matrix x with null as default or inverted
  set <- function(y) { x <<- y
    inverted <<- NULL
  } 
  ## calling set(matrix)  assigns the matrix to x
  getmatrix <- function() x
  ## recovers the matrix
  setinverted <- function(inv) inverted <<- inv 
  ## sets inverted matrix value
  getinverted <- function() inverted 
  ## gets inverted matrix value
  list(set=set, getmatrix = getmatrix,
       setinverted = setinverted,
       getinverted = getinverted) 
  ## creates list of functions accessible via $
}


## this functions solves the inverse but checks for a cached value first 

cacheSolve <- function(x, ...) {
   inv <- x$getinverted() 
   ## gets any cached values
   if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  } ## if such value, returns it
  data <- x$getmatrix()
  inv <- solve(data) 
  ## solves if null cache 
  x$setinverted(inv)
  inv     ## Return a matrix that is the inverse of 'x'
}
