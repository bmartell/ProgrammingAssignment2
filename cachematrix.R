## This function attaches some functions to a matrix 
## in order to cache values associated to it

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set <- function(y){
    x<<-y
    m<<-NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This function computes the inverse of the matrix 
## given as input parameter and caches it. If there
## is a cached inverse matrix already it simply reads
## the cached object and gives an informational message

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
