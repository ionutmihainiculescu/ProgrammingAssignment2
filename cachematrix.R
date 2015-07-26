## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## stores matrix and matrix inverse

makeCacheMatrix <- function(x = matrix()) {
     if(!exists("m")){
          m <<- NULL
          inverse <<- NULL
     }
     set <- function(y) {
          x <<- y
          inverse <<- NULL
     }
     setInverse <- function(ss){
          m <<- x
          inverse <<- ss
     }
     get <- function() x
     getM <- function() m
     getInverse <- function() inverse
     list(get = get, getM = getM, getInverse = getInverse, set = set, setInverse = setInverse)

}


## Write a short comment describing this function

## checks whether there is matrix, if not calculated is solution
## If the matrix is identical, solution has already calculated is return
## if it is not identical, the solution is computed, stores and return

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     d <- x$get()
     m <- x$getM()
     s <- NULL
     if(is.null(m)){
          ss <- solve(d)
          x$setInverse(ss)
          s <- x$getInverse()
     }
     else{
          if(identical(d, m)){
               s <- x$getInverse()
          }
          else{
               x$setInverse(solve(d))
               s <- x$getInverse()
          }
     }
     s
}
