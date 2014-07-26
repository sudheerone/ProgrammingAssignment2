## makeCacheMatrix: creates special 'matrix' object that will cache the inverse of the matrix
## cacheSolve: computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.


## creates a cache matrix object.
## Input: matrix ,to which inverse to be cached.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Calculates the inverse of the matrix.
## if cached inverse value is available it returns the cached value 
## otherwise it calculates the inverse and returns.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
          message("getting cached data")
          return(m)
        }
        ## if not cached, calculate the inverse and cache
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
