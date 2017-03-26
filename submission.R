makecachematrix <- function(x = matrix()) {
  m <- NULL
  set.Mat <- function(y) {
    x <<- y
    m <<- NULL
  }
  get.Mat <- function() x
  set.Cache <- function(inverse) m <<- inverse
  get.Cache <- function() m
  list(set.Mat = set.Mat, 
       get.Mat = get.Mat,
       set.Cache = set.Cache,
       get.Cache = get.Cache)
}

cacheSolve <- function(x, ...) {
  m <- x$get.Cache()
  if(!is.null(m)) {
    message("getting cached matrix")
    return(m)
  }
  dMat <- x$get.Mat()
  m <- solve(m, ...)
  x$set.Cache(m)
  m
}
