##setwd("E:/Data Science Certification/R_Programming/GitHub")
##source("cachematrix.R")

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


##This function computes the inverse of the special "matrix" returned 
##by the makeCacheMatrix function. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the inverse should be retrieved from the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

##Example:
##> x <- matrix(c(2,4,1,-1,1,-1,1,4,0), nrow=3,ncol=3,byrow=TRUE)
##> x
##[,1] [,2] [,3]
##[1,]    2    4    1
##[2,]   -1    1   -1
##[3,]    1    4    0
##>m <- makeCacheMatrix(x)
##>s <- cacheSolve(m)
##> s
##[,1] [,2] [,3]
##[1,]   -4   -4    5
##[2,]    1    1   -1
##[3,]    5    4   -6
