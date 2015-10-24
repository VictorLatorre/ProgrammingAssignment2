## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


#The first function, makeCacheMatrix creates a special "vector", which is really a list containing a function to
#
# set the value of the matrix
# get the value of the matrix
# set the value of the mean
# get the value of the mean

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

# The following function calculates the inverse of the matrix created with the makeCacheMatrix function. 
# Firstly checks if the matrix inverse is already in cache. If so, it gets the matrix from 
# the cache and skips the computation. Otherwise, using the above functions proceed to calculate the inverse
# in the cache via the functions solve and setmatrix.

cacheSolve <- function(x, ...) {
    m<-x$getmatrix()
    if(!is.null(m)){
      message("getting cached data")
      return(m)
    }
    matrix<-x$get()
    m<-solve(matrix, ...)
    x$setmatrix(m)
    m
  }
        ## Return a matrix that is the inverse of 'x'
