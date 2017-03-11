## This function returns  a new "matrix", and saves a cache matrix produced within.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
     set <- function(y) {
         x <<- y
         m <<- NULL
     }
     get <- function() x
     setsolve <- function(solve) m <<- solve
     getsolve <- function() m
     list(set = set, get = get,
          setsolve = setsolve,
          getsolve = getsolve)
}



## This function creates the value of the inverse of a "matrix" returned by the function above,and it will return 
## "getting cached data" when the original matrix(x) repeats, and save the computing time as we do not need to recalculate.       
      
cacheSolve <- function(x, ...) {
       m <- x$getsolve()
     if(!is.null(m)) {
         message("getting cached data")
         return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setsolve(m)
     m
}

