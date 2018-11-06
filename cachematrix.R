## The functions makeCacheMatrix and cacheSolve, takes a matrix, calculates and cache
## its inverse. These functions are written based on assumption that matrix supplied 
## is always invertible.

## makeCacheMatrix function creates a special type of "matrix" which is basically a
## list containg the functions to: set the value of actual matrix, get the value of
## actual matrix, set the value of inverted matrix and get the value of inverted
## matrix
##
## Usage mx<-makeCacheMatrix(matrix(c(1,2,3,4),nrow=2,ncol=2))
##       names(mx)
##       [1] "setActualMatrix"  "getActualMatrix"  "setInverseMatrix" "getInverseMatrix"

makeCacheMatrix <- function(actualMatrix=matrix()) {
  
  # intialize inverse of matrix
  inverseMatrix <- NULL
  
  # function to set the value of provided matrix
  setActualMatrix <- function(m) {
    actualMatrix <<- m
    inverseMatrix <<- NULL
  }
  
  # function to get the value of provided matrix
  getActualMatrix <- function() actualMatrix
  
  # function to set the value of inverse matrix
  setInverseMatrix <- function(im) inverseMatrix <<- im
  
  # function to get the value of inverse matrix
  getInverseMatrix <- function() inverseMatrix
  
  # return the list of functions
  list(setActualMatrix = setActualMatrix, 
       getActualMatrix = getActualMatrix,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}


## cacheSolve function takes a speacial type of "matrix" makeCacheMatrix and returns
## its inverse if it exists in the cache. If the inverse of the matrix does not exits
## then it calculates its inverse and saves it in the cache.
## This function assumes that the matrix passed is always an invertible matrix.
##
## Usage: mx<-makeCacheMatrix(matrix(c(1,2,3,4),nrow=2,ncol=2))
##        cacheSolve(mx)
##             [,1] [,2]
##        [1,]   -2  1.5
##        [2,]    1 -0.5

cacheSolve<-function(cacheMatrix, ...) {
  
  # get Inverse Matrix
  im <- cacheMatrix$getInverseMatrix()
  
  # if inverse matrix exists
  if(!is.null(im)){
    print("getting cached inverted matrix")
    return(im)
  }
  
  # inverse matrix does not exist, get the actual matrix
  m <- cacheMatrix$getActualMatrix()
  
  # calculate inverse matrix
  im <- solve(m, ...)
  
  # cache the inverted matrix
  cacheMatrix$setInverseMatrix(im)
  
  # return the inverted matrix
  im
}
