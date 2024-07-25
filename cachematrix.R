## The makeCacheMatrix creates a cache for a matrix and then cacheSolve uses 
## the stored matrix to calculate the inverse of that matrix
## and caches that as well so that it is easier to access the inverse of the
## initial matrix

## in the makeCacheMatrix we establish that x will be a matrix; the inverse of 
## the matrix will be stored in inv. We then have 4 functions (set, get, setinv 
## and getinv) that are stored in a list which is used in the next function 
## (cacheSolve). Set() sets the new matrix and makes sure that the 
## inv value is reset. Get() gets the values for the current matrix. Setinv()
## sets the calculated inverse which is passed on to inv for storage and then
## getinv() returns the cached inv

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL 
  set <- function(y){ 
        x <<- y
        inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv= getinv)
}


## we use x$getinv() to look for the cached inv of the previous function. If it 
## already exist, then that value is returned (return(inv)). If there isn't a 
## cached inv, then it's calculated with solve(), cached into x$getinv and 
## then returns a matrix that is inversed from the original matrix "x".

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)){
      message("getting cached data")
      return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv ## Return a matrix that is the inverse of 'x'
}



