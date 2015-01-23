## The following functions work in conjunction to take advantage of ability 
## to cache R objects.  

## To do so, the first function takes as an the input a matrix and returns a 
## list of functions we'll need to find the inverse.  The second function calls
## the first function to create a non-atomic R object to which $ can apply.  
## Because the original atomic matrix "x" has been "cooked into" the non-atomic
## CachedMatrix object, the result follows.  

## This function returns a list of functions to:
##   (1) set a matrix, 
##   (2) get the matrix, 
##   (3) set the inverse of the first matrix, and 
##   (4) get that inverse

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y) {
          x <<- y
          inv <<- NULL
     }
     get <- function() x
     setinverse <- function(solve) inv <<- solve
     getinverse <- function() inv
     list(set = set, get = get, 
          setinverse = setinverse, 
          getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     ## To avoid that awful "$ operator is invalid for atomic vectors" 
     ## I assigned the non-atomic result of the makeCacheMatrix function to x.
       
     mat <- makeCacheMatrix(x)
     cx <- solve(x)
     mat$setinverse(cx)
     x <- mat
     
     inv <- x$getinverse()
     if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
     }
     data <- x$get()
     inv <- solve(data)
     x$setinverse(inv)
     inv
}
