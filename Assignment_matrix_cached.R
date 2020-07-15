makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) { 
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmat <- function(solve) m <<- solve
  getmat <- function() m
  list(set = set, get = get,setmat = setmat,getmat = getmat)
}

cacheSolve <- function(x, ...) {
  m <- x$getmat()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$mat()
  m <- mean(data, ...)
  x$setmat(m)
  m
}

