# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  # set the value of the matrix
  
  set <- function(y) {
    x <<- y
    m<<- NULL
  }
  
  # get  the value of the matrix
  
  get <- function() x
  
  
  # caching the inverse matrix in another environment
  # to be reused without recomputing
  
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should retrieve the 
# inverse from the cache.

cacheSolve <- function(x, ... ) {
  
  m <- x$getinverse()
  
  # If the inverse has been calculated (not NULL), 
  # return the cached data
  
  if(!is.null(m)) {
    
    message("getting cached data")
    return(m)
    
  }
  
  # Else, calculate the inverse
  data <- x$get ()
  
  # Computing the inverse of a square matrix
  # returns its inverse.
  
  m <- solve(data, ... )
  x$setinverse(m)
  m
}

