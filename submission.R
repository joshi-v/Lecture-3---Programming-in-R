## The makeCacheMatrix creates a list of objects, which are functions. the 
# set/setinv function pair assigns the matix value to the cached environment. 
# That hidden env stores the matrix and its inverse.

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

## The cacheSolve function takes the chached matrix, and performs operations 
# on that. If the matrix has a precalculated inverse matrix, then that is returned
# If the inverse has not been calculated yet, than calculate and store the matrix 
# inverse in the "cached matrix"

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