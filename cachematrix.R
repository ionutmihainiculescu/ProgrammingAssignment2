## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     sl <- NULL
     set <- function(y) {
          x <<- y
          sl <<- NULL
     }
     setS <- function(ss){
          m <<- x
          sl <<- ss
          sl
     }
     get <- function() x
     getM <- function() m
     getSolve <- function() sl
     list(get = get, getM = getM, getSolve = getSolve, set = set, setS = setS)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     d <- x$get()
     m <- x$getM()
     s <- NULL
     if(is.null(m)){
          ss <- solve(d)
          x$setS(ss)
          s <- x$getSolve()
          s
     }
     else{
          if(identical(d, m)){
               s <- x$getSolve()
          }
          else{
               x$setS(solve(d))
               s <- x$getSolve()
          }
     }
     s
}
