## R Programming - Coding Assignment 2: Lexical Scoping

## This function creates an object of type 'matrix'
## and supports initial defintiion and look up of the 
## matrix via 'set()' and 'get()' operators. This function
## can also be used to directly define (and cache) the
## inverse of a matrix via 'setinv()'. Look up of the 
## matrix inverse is supported via 'getinv()'

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    
    x<<-y
    m<<-NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get, setinv = setinv, getinv = getinv)

}

## This funcion can be used to find the inverse of a 
## cached matrix defined using makeCacheMatrix() function.
## If the inverse is already defined, it returns a message
## indicating that is has pulled the cached inverse value.

cacheSolve <- function(x=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setinv(m)
  m
}

## This way of defining a cache function seems potentially dangerous.
## Since I can define the inverse straigth away via 'setinv()' it seems
## like a user could erroneously set such a value and then use an incorrect
## matrix inverse value in future calculations.
