makecachematrix <- function(x = matrix()) {
  m <- NULL
  setMat <- function(y) {
    x <<- y
    m <<- NULL
  }
  getMat <- function() x
  setCache <- function(inverse) m <<- inverse
  getCache <- function() m
  list(setMat = setMat, 
       getMat = getMat,
       setCache = setCache,
       getCache = getCache)
}

cacheSolve <- function(x, ...) {
  m <- x$getCache()
  if(!is.null(m)) {
    message("getting cached matrix")
    return(m)
  }
  dMat <- x$getMat()
  m <- solve(m, ...)
  x$setCache(m)
  m
}