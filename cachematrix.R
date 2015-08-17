## 17 Aug - MB: Assignment 2 for RProg-031 (v0.1)
## These functions cache the inverse of a matrix to reduce costly computations

## creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix())  {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}


## Return the inverse of a "matrix". 
## NB If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}
