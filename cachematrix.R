## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##The function makeCacheMatrix creates a special matrix object that has the 
##capacity to cache its inverse thus saving computational time
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function()x
  setInverse<- function(solve) m<<-solve
  getInverse<- function()m
  list(set = set,get = get,setInverse = setInverse,getInverse = getInverse)
}


## Write a short comment describing this function
##This function computes the inverse IF there is no previous inverse 
##in the cache memory from makeCacheMatrix Function
cacheSolve <- function(x, ...) {
  m<-x$getInverse()
  if(!is.null(m)){
    message("Getting cached data")
    return(m)
  }
  data<-x$get()
  m<- solve(data,...)
  x$setInverse(m)
  m
  ## Return a matrix that is the inverse of 'x'
}
