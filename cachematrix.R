## Overall, this function allows you to set a cached matrix that's inverse can be
## solved and cached for efficiency.

## This function makes a matrix with 4 behaviors that can retrieve and set the data.

makeCacheMatrix <- function(x = matrix()) {
  +     inv <- NULL
  +     set <- function(y) {
    +         x <<- y
    +         inv <<- NULL
    +     }
  +     get <- function() x
  +     setInverse <- function() inv <<- solve(x)
  +     getInverse <- function() inv
  +     list(set = set, get = get,
             +          setInverse = setInverse,
             +          getInverse = getInverse)
}


## The cacheSolve fuction can retrieve the object set in makeCacheMatrix
## it into the parent environment. "inv" is set to NULL as default.

cacheSolve <- function(x, ...) {
  +     inv <- x$getInverse()
  +     if(!is.null(inv)) {
    +         message("getting cached data")
    +         return(inv)
    +     }
  +     mat <- x$get()
  +     inv <- solve(mat, ...)
  +     x$setInverse()
  +     inv
}
