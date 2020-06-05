## makecachematrix creates a matrix object than can cache its inverse
## makecachematrix essentially creates and returns a list of functions 

## This can be used by the Cachesolve function to either set or get the inverse of the matrix in cache

makeCacheMatrix <- function(x = matrix()) {
## initialise value to NULL
  ## make sure cached value is stored
  m_cache <- NULL
 ## create matrix in working environment
   set <- function(y) {
    x <<- y
    m_cache <<- NULL
   }
   ## get the value of the matrix 
   get <- function() x
   ## invert the matrix and store it in cache
   setinverse<- function(inverse) m_cache <<- inverse
   ##obtain the inverted matrix from cache 
   getinverse <- function() m_cache
   
## return functions to working environment 
   list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}

##cacheSolve returns the inverse of the matrix created in cachematrix
## if the matrix does not exist in cache it's created in the working environment
## It's inverse is also created and then stored in cache 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m_cache <- x$getinverse() ## attempt to get inverse stored in cache
  if(!is.null(m_cache)) {
    message("getting cached data")
    return(m_cache)
  }
  data <- x$get() ##create matrix since/if it doesn't exist
  m_cache <- solve(data, ...)
  x$setinverse(m_cache)
  m_cache
}

