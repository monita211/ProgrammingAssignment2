##Together these functions will first cache the inverse of a matrix. Then, they will determine if there is a cached value. If there is,
##the value is returned. If not, the inverse is calculated and returned.

## makeCacheMatrix will store in a list set, get, setinverse and get inverse. Set will store in the main function the value passed to it and 
##then empty the matrix. Get will return the value passed to the main function. setinverse will assign to the empty matrix the inverse. getinverse
#will return the solved matrix.

makeCacheMatrix <- function(x = matrix()) {
  n <- NULL 
  set <- function(y) { 
    x <<- y  
    n <<- NULL 
  }
  get <- function() x 
  setinverse <- function(solve) n <<- solve 
  getinverse <- function() n 
  list(set = set, get = get,  
       setinverse = setinverse,
       getinverse = getinverse)
}

#cacheSolve will return the value of the inverse if already stored by the makeCacheMatrix function. If there is no stored value, this function
#calculates the inverse and returns it.

cacheSolve <- function(x, ...) {
  n <- x$getinverse() 
  if(!is.null(n)) {  
    message("getting cached data")
    return(n)
  } 
  data <- x$get() 
  n <- solve(data, ...) 
  x$setinverse(n) 
  n 
}

