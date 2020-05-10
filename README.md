# R-programming-course
Programming assignment 2
##Assignment 2##
#First Function#
#This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix<- function(x=matrix()) {
  
  inver <- NULL
  set <- function(y) {
    x <<- y
    inver <<- NULL 
  }
  get <- function() x
  setinver <- function(inverse) inver <<- inverse
  getinver <- function() inver
  list(set=set, get=get, setinver=setinver, getinver= getinver)
}

#Second Function#
#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve<- function(x, ...) {
  inver <- x$getinver()
  if(!is.null(inver)) {
    message("getting cached result")
    return(inver)
  }
  data <- x$get()
  inver <- solve(data, ...)
  x$setinver(inver)
  inver
}

### Example ###
set.seed(1245)
mm<-matrix(rnorm(4),2,2)
mm
mmm<- makeCacheMatrix(mm)
cacheSolve(mmm)
solve(mm)
