  ## functions do
  
  ## makeCacheMatrix
  ## input: a matrix
  ## outout: a matrix
  ## purpose: stores a matrix into cache, 
  ##          and retrieves a matrix from cache
  ## functions: 
  ##   set: sets the value 
  ##   get: retreives the value of a matrix from memory
  ##   setInverse: sets the function to perform
  ##   getinverse: gets the function to perform
  makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) {
      m <<- inverse
    }
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  }
  
  ## makeCacheMatrix
  ## input: a function like makeCacheMatrix
  ## outout: Return a matrix that is the inverse of 'x'
  ## purpose: Takes in a matrix, checks to see if its cached...
  ##          if it is not cached performs a inverse on hte matrix
  ## functions: 
  ##  
  cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    
    ##get our data from o
    data <- x$get()
    
    ##Validation
    ##  Make Sure Matrix exists and is passed in'
    if (is.null(data)){
      message('please pass in a matrix to invert')
      return(m)
    }
    
    ##  Make Sure Matrix is a square or else we cannot calculate
    if (ncol(data) != nrow(data))
    {
      message('in order to inverse a matrix needs to be a square')
      return(m)
    }
    
    ## calculate invers
    m <- solve(data, ...)
    
    ## store results into a cache
    x$setinverse(m)
    
    ## return results
    m
  }