## cacheMatrix.R
## 24SEP2014, Created
## Mike S.
###################################################################
## Program consists of two functions: makeCacheMatrix & cacheSolve
## These functions create a special matrix object that can cache it's
## inverse, then after checking if the solution is already available,
## calculates and/or returns the matrix inverse.
###################################################################

## function creates a matrix, sets its value and inverse.

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

## function computes inverse of matrix, check whether inverse is cached
## and returns inverse.

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("Getting Cached Data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}
