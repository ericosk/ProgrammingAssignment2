## Objective : avoid calculating an inverse matrix, if already exists
## Methodology : create a special "matrix" object that caches it's inverse
## This "special matrix" contain functions to check if the inverse already 
## exists (in the cache), if so return it ; if not, solve it and cache it for next calls

## Write a short comment describing this function

## makeCacheMatrix :  return a list containing functions to set, get either the
##                    matrix or it inverse

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL             ## stores the inverse of the matrix
  set <- function(y) {
    x <<- y             ## initialize the matrix with the value passed by user
    i <<- NULL          ## as x is newly created here, set it's inverse to NULL
  }
  get <- function() x   ## just return the value of the object, when called
  setinverse <- function(inv) i <<- inv ## the inverse is calculated (outside) and set here
  getinverse <- function() i               ## just return the inverse (i)
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)           ## when the funtion is call, it return this list
                                          ## of 4 functions

}


## cacheSolve :   call for the "special matrix" object and attributes
##                performs checks to see if it's need to re-run the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse() ## try to get the inverse of the matrix by call the special
                      ## function getinverse of the object "x"
  if(!is.null(i)) {
    message("getting cached data")
    return(i)         ## i is not null, so inverse already created before, so return it
                      ## and quit the function here (no need to proceed further)
  }
  data <- x$get()     ## if we are here, it's because the inverse was not found, so we
                      ## need to get the matrix's data again and solve it's inverse for 
                      ## futur calls
  inv <- solve(data, ...)   ## here we calculate the inverse
  x$setinverse(inv)         ## here we cache it in the "special matrix"
  inv                       ## finally we return the newly calculated inverse
}
