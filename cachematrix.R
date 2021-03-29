## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix creates a matrix with set and get so when you want to again get the inverse of the matrix
## it takes it from cache instead of recalculating everything

makeCacheMatrix <- function(mx = matrix()) {
  # set inverse to Null
  inv <- NULL
  # set matrix
  set <- function(mrix){
    mx <<- mrix
    inv <<- NULL
  }
  # get matrix
  get <- function() mx
  # set inverse
  setInverse <- function(solvemx) inv <<- solvemx
  # get inverse
  getInverse <- function() inv
  # return list with set and get
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve calculates inverse of matrix and if already calculated it returns the cache instead of recalculating

cacheSolve <- function(cachedmx, ...) {
        # get cached inverse
        inv <- cachedmx$getInverse()
        # when yes return cache
        if (!isnull.(inv)){
          message("Loading cached data")
          return(inv)
        }
        # if not calculate and return
        origmx <- cachedmx$get()
        inv <- solve(origmx, ...) # inverse origmx
        cachedmx$setInverse(inv)
        inv # return inverse
}
