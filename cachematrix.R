## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#This function creates a list contains functions for set/get the matrix and setInvMat/getInvMat 
#to get/set inverion of the matrix
makeCacheMatrix <- function(x = matrix()) {
  #the variable that holds the inversion of the matrix
  invm <- NULL

  #set/get for the matrix
  set <- function(y) {
    x <<- y
    invm <<- NULL
  }
  get <- function() x
  
  #set/get for the inversion of the matrix
  setInvMatrix <- function(m) {
    invm <<- m
  }
  getInvMatrix <- function() invm
  
  list(set = set, get = get, setInvMat = setInvMatrix, getInvMat = getInvMatrix)
}


#A function takes x argument which is created by makeCacheMatrix function 
#and return the inversion of the matrix object
cacheSolve <- function(x, ...) {
    #try to get the invMat and see if its not 
    #null then we return it
    invMat <- x$getInvMat()
    if(!is.null(invMat)) {
      return(invMat)
    }
    
    #if the invMat is null then calculate
    #the inversion and cache it in the x object
    m <- x$get()
    invMat <- solve(m)
    x$setInvMat(invMat)
    invMat
}
