## Put comments here that give an overall description of what your
## functions do
## ---------------------------------------------------------------------------
## These two functions are used cuncurrently to find the inverse of a matrix x.
## If the inverse has been found already, it is returned without recalculating.
## makeCacheMatrix() has to be called prior to cacheSolve().
## ---------------------------------------------------------------------------
## Write a short comment describing this function
## ---------------------------------------------------------------------------
## This function creates a special R object that: 
## Initializes a variable 'm', which will be used to save inverse matrix.
## Provides function get() to obtain "raw" matrix 
## Provides function setImatrix() to assign computed inverse matrix (of x) to m
## Provides function getImatrix() to obtain the cached inverse matrix.
## ---------------------------------------------------------------------------

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


## Write a short comment describing this function
## ---------------------------------------------------------------------------
## This function does the actual inversing of matrix x.  
## If the inverse matrix has been found already,returns the cached result
## If no cached data found,inverse of x is calculated, saved to cached, 
## and returned.
## ---------------------------------------------------------------------------

cacheSolve <- function(x=matrix(), ...) {
## Return a matrix that is the inverse of 'x'
    m<-x$getmatrix()
    if(!is.null(m)){
      message("getting cached data")
      return(m)
    }
else {
    message("no cached data found.Calculating Inverse Matrix.. ")
    matrix <- x$get() 
    m<-solve(matrix, ...)
    x$setmatrix(m)
    message("done.. ")
    m
   }
}

