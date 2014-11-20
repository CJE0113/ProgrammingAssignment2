## Function: makeCacheMatrix
## Input: numeric matric - default to empty matrix
## Output: List of 4 functions
## Purpose: Creates an environment with a numeric matric and the associated
##          functions required to effectively cache its inverse

## Function: set
## Input: numeric matrix
## Output: null
## Purpose: Allows other environments to change the data stored in 
##          environment and resets the cached inverse to null

## Function: get
## Input: None
## Output: numeric matrix
## Purpose: Allows data from an object created with makeCacheMatrix to be
##          accessed by other functions

## Function: setInverse
## Input: Inverse of matrix stored as x in this environment
## Output: inverse (not used)
## Purpose: Allow other functions to store inverse matrix within the 
##          environment

## Function: getInverse
## Input: None
## Output: Inverse Matrix
## Purpose: Allow other functions to access inverse matrix within the 
##          environment


makeCacheMatrix <- function(x = matrix()) {
  ## Creating caching environment for first time, inverse hasn't been calc'ed
  inverse <- NULL
  
  set<- function (y){
    x <<- y
    ## Invalidate any cached inverse values
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function (i) inverse <<- i
  getInverse <- function () inverse
  ## return a list to allow functions within this environment
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
    

}


## Function: cacheSolve
## Input: List of functions returned by makeCacheMatrix 
## Output: inverse matrix
## Purpose: Checks if inverse has been calculated, if so, returns it.
##          Otherwise, calculates the inverse, stores it in environment
##          the object passed was declared in and returns the inverse.

cacheSolve <- function(x, ...) {
  ## get inverse from environment of x
  inverse = x$getInverse()
  
  ## check if it has already been calculated
  if (!is.null(inverse)){
    return(inverse)
  }
  
  ## if not, get the data stored in x
  data <- x$get();
  
  ## calculate the inverse
  inverse <- solve(data, ...)
  
  ## set the inverse object in x's envir. to the calculated value
  x$setInverse(inverse)
  
  ## return the inverse
  inverse
}
