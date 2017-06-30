##Function for MakecaheMatrix:
##The Function has been replaced "solve" instead of "mean" like Example:


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Function for cachesolve

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if (!is.null(m)) {
    message("get the Cached Matrix")
    return(m)
  }
  mat <- x$get()
  m <- solve(mat, ...)
  x$setInverse(m)
  m
}

