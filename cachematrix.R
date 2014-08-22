## The two functions below computes inverse of a matrix and
## stores it to access whenever inverse of the same matrix is required

## This function creates an object that returns a list containing methods
## to set and get the inverses of a matrix.

makeCacheMatrix <- function(x = matrix()) {
  Inv <- NULL
  set <- function(y) {
    x <<- y
    Inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) Inv <<- inverse
  getinverse <- function() Inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function basically checks if the inverse of a matrix is already
## computed. If yes, it simply gets it from above function, else it computes

cacheSolve <- function(x, ...) {
  Inv <- x$getinverse()
  if(!is.null(Inv)) {
    message("getting cached data")
    return(Inv)
  }
  mat <- x$get()
  Inv <- solve(mat)
  x$setinverse(Inv)
  Inv         ## Returns a matrix that is the inverse of 'x'
}