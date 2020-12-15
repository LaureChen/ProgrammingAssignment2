## The following function 'makeCacheMatrix' creates a matrix whose
## objects are cached inversely. It is a list containing a function
## to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
inv = NULL
set = function(y){
  x <<- y
  inv <<- NULL
}
get = function() x
setinv = function(inverse) inv <<- inverse
getinv = function() inv
list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The following function 'cacheSolve' gives the inverse of the
## matrix returned by the last function 'makeCacheMatrix'. If the
## inverse has already been calculated and there is no change in
## the matrix, this function should retrieve the inverse from the 
## cache. 

cacheSolve <- function(x, ...) {
  inv = x$getinv()
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat.data = x$get()
  inv = solve(mat.data, ...)
  return(inv)
}
