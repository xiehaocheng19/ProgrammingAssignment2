## Put comments here that give an overall description of what your
## functions do

## We will write functions to cache the inverse of a matrix rather
##than compute it repeatedly

## Write a short comment describing this function
## Creates a special "matrix" object that can cache its inverse
##with get, set, setinv, getinv functions

makeCacheMatrix <- function(x = matrix()) {
  inv<- NULL
  set<- function(y) {
    x<<- y
    m<<- NULL
  }
  get<- function() x
  setinv<- function(number) inv<<- number
  getinv<- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  
}


## Write a short comment describing this function
## Computes the inverse of the special "matrix" returned by 
##makeCacheMatrix above. If the inverse has already been calculated
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<- x$getinv()
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data<- x$get()
  inv<- solve(data, ...)
  x$setinv(inv)
  inv
  
}
