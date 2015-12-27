## two functions below caching a inverse of the matrix and it is not recomputed again, when it needed

## Create a special matrix that can cashe its inverse

makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL
    set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setrev <- function(solve) inv <<- solve
  getrev <- function() inv
  list(set = set, get = get,
       setrev = setrev,
       getrev = getrev)
}



## Computes the inverse of the special matrix returned by makeCacheMatrix. If the reverse 
## has already been calculated (and the matrix has not changed) then the cachesolve 
## should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  inv <- x$getrev()
  if(!is.null(inv)) {
      message("getting cached reverse matrix")
      return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setrev(inv)
  inv
}
