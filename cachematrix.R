## These functions calculate the inverse of a matrix and store it
## in a cache so if it is needed again it can be retrieved from the cache
## instead of being reprocessed each time.

## makeCacheMatrix makes a list of functions that will:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

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


## This function returns the inverse of the matrix by checking whether the inverse has already
## been created and stored in a cache. If so it will retrieve it without reprocessing the task. 
## If not it will process the task and store the result in a cache for later retrieval

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("retrieving cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}