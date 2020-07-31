## Put comments here that give an overall description of what your
## functions do
   ##This code transforms a regular matrix into a special type of matrix, which
   ##contains more information about it, making it faster to get its inverse.

## Write a short comment describing this function
   ##This function returns a list of functions that allows the user to 
   ##handle better with it. This way, the user can modify its content or its inverse
   ##and also get these values. This makes the computation faster if the user has
   ##already calculated the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
   inverse <- NULL
   set <- function(value){
      x <<- value
      inverse <<- NULL
   }
   get <- function() x
   setInverse <- function(inverseValue) inverse <<- inverseValue
   getInverse <- function() inverse
   list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
   ##This function calculates the inverse matrix faster than the 
   ##standard function solve(), because it checks first if it's been
   ##calculated before. If so, the function gets the stored value and returns it.
   ##Otherwise, it calculates the inverse matrix and storages it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   inverse <- x$getInverse()
   if(is.null(inverse)){
      inverse <- solve(x$get(), ...)
      x$setInverse(inverse)
      return(inverse)
   }
   inverse
}
