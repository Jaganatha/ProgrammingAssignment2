##Function for MakecaheMatrix:
##The Function has been replaced "solve" instead of "mean" like Example:


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {                #function to set the value of Matrix x
    x <<- y
    m <<- NULL
  }
  get <- function() x                 #function to get the value of Matrix x
  setInverse <- function(inverse) m <<- inverse  #function to set the Inverse of Matrix x
  getInverse <- function() m    # Function to get the Inverse of Matrix x
  list(set = set,               #list containing set, get, setInverse,getInverse
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Function for cachesolve

cacheSolve <- function(x, ...) {
  m <- x$getInverse()              #getting Inverse of x if already available
  if (!is.null(m)) {                 #using if statement to check whether the vector m contains Inverse matrix or NULL
    message("get the Cached Matrix")
    return(m)                              
  }
  mat <- x$get()            #if Inverse is not available , calculation is done by fetching the matrix mat
  m <- solve(mat)            #computing the inverse of matrix usnig Solve()
  x$setInverse(m)
  m                          #outputs Inverse
}

