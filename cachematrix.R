##  makeCacheMatrix creates a special "vector", which is really a list containing a function

makeCacheMatrix <- function(x = matrix()) {
  ma <- matrix()
  
  ## set the value of the matrix  
  set <- function(y) {
    x <<- y
    ma <<- matrix()
  }
  
  ## get the value of the matrix
  get <- function() x
  
  ## set the value of the inverse
  setinv <- function(inv) ma <<- inv  
  
  ## get the value of the inv
  getinv <- function() ma  
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


##The following function calculates the inv of the special "vector" created with the above function. 

cacheSolve <- function(x) {
  ma <- x$getinv()  
  
  ##Checks to see if the inv has already been calculated.
  if(!is.null(ma)) {
    message("getting cached data")
    return(ma)  ## If so, it gets the inv from the cache and skips the computation. 
  }
  data <- x$get()
  ma <- solve(data)  ##Otherwise, it calculates the inv of the data 
  x$setinv(ma)  ##and sets the value of the inv in the cache via the setinv function.  
  ma
}