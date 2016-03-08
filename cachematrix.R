# makeCacheMatrix is a function that returns a list of functions
# Its puspose is to store a martix and a cached value of the inverse of the 
# matrix. Contains the following functions:
# set:sets the value of matrix
# get:gets the value of matrix
# setinverse:sets the value of inverse matrix
# getinverse:gets the value of inverse matrix



makeCacheMatrix <- function(x = matrix()) {
  
  inv<-NULL
  set<-function(y)
  {
    x<<-y
  inv<<-NULL
  }
  get<-function() x
  setinverse<-function(inverse) inv<<-inverse
  getinverse<-function() inv
  list(set= set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)

}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data,...)
  x$setinverse(inv)
  inv
}
