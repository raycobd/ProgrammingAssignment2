## Matrix inversion improved performance by caching 
## the inverse of a matrix rather than compute it repeatedly. 
##
## FUNCTIONS:
##  makeCacheMatrix
##  cacheSolve
##

##
## This function creates a special "matrix" object that can cache its inverse.
## 
## RETURN:
##  list
##      set
##      get
##      setInverse
##      getInverse
##
makeCacheMatrix <- function(x = matrix()) {
  mInversed <- NULL
  set <- function(y) {
    x <<- y
    mInversed <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) mInversed <<- inverse
  getInverse <- function() mInversed
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


##
## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## then the cachesolve should retrieve the inverse from the cache
## 
## RETURN:
##  reverse matrix allocated in x
##
cacheSolve <- function(x, ...) {
  mInversed <- x$getInverse()
  if(!is.null(mInversed)) {
   # message("getting cached data")
    return(mInversed)
  }
  data <- x$get()
  mInversed <- solve(data)
  x$setInverse(mInversed)
  # message("generating data")
  mInversed
}


 
