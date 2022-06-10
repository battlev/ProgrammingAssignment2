## Takes an argument x, which is by default, a null 
## matrix, and returns a list of functions which
## cache the matrix value and its inverse.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL ## inverse is initially null
  set <- function(y){ 
    x <<- y
    i <<- NULL
  }
  get <- function() x ## returns current matrix value
  setInverse <- function(inverseMatrix) i <<- inverseMatrix ## sets inverse value
  getInverse <- function() i ## returns inverse value
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Takes an argument x, which should be the 
##list from the previous function.Attempts to 
## retrieve the cached inverse matrix value.
## If no value is currently cached, calculates 
## matrix inverse and stores in the given list. 
cacheSolve <- function(x, ...) {
    currentInverse = x$getInverse() ## get current inverse value 
    if(!is.null(currentInverse)){
      message("Retrieving cached data!")
      return(currentInverse)
    }
    currentMatrix <- x$get()
    i <- solve(currentMatrix)
    x$setInverse(i)
    i
    ## Return a matrix that is the inverse of 'x'
}
