## Put comments here that give an overall description of what your
## functions do

## This function takes a matrix and returns 
## a CacheMatrix that is used to cache the inverse of a matrix.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function takes in a CacheMatrix that we make with the function
## that we defined above, if we have calculated the inverse of the
## CachedMatrix, then it gets the cached data of the inverse, if not
## it computes the inverse of the CachedMatrix and save it into the "object".
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

###### VALIDATION #################
# mat <- matrix(rnorm(25),5,5)
# cachedmat <- makeCacheMatrix(mat) 
# cacheSolve(cachedmat)
# all(solve(mat) == cachedmat$getinverse())
