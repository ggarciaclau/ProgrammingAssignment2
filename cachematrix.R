## Put comments here that give an overall description of what your
## functions do

## With the makeCacheMatrix function, we can set and get a matrix and 
## also get its inverse if already been calculated with the cacheSolve 
## function, if not, we will be returned: NULL.

## The cacheSolve function computes the inverse of a matrix (created 
## with the above function). If already has been calculated the inverse, 
## we will be returned the message "getting cached data".


## Write a short comment describing this function
## In this function, the double arrow operator is used to assign a value 
## to an object in an environment (makeCacheMatrix environment) that 
## is different from the current environment (cacheSolve environment).

makeCacheMatrix <- function(x = matrix()) {
  inve<-NULL
  set<-function(y){
    x<<-y
    inve<<-NULL
  }
  get<-function(){x}
  setInverse<-function(inverse){inve<<-inverse}
  getInverse<-function(){inve}
  list(set=set,get=get,
       setInverse=setInverse,
       getInverse=getInverse)
}


## Write a short comment describing this function

## This function works in conjunction with the previous one, and 
## it will compute the inverse of a matrix, but first, the function 
## will check if the matrix has been already calculated.

cacheSolve <- function(x, ...) {
  inve<-x$getInverse()
  if(!is.null(inve)){
    message("getting cached data")
    return(inve)
  }
  data<-x$get()
  inve<-solve(data,...)
  x$setInverse(inve)
  inve
        ## Return a matrix that is the inverse of 'x'
}
