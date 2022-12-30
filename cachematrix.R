## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInv <- function(solve) i <<- solve
  getInv <-  function() i
  list(set=set, get=get, 
       setInv=setInv, 
       getInv=getInv)
}

## Returns a matrix that is the inverse of inputted matrix x
cacheSolve <- function(x, ...) {
    i <- x$getInv()
    if(!is.null(i)){
      message("getting cached matrix")
      return (i)
    }
    data <- x$get()
    i <- solve(data,...)
    x$setInv(i)
    i
}
