## These functions take in a matrix, and find the inverse if it has not already been cached. if it has not been cached, the inverse will be found and cached.


## this function generates a list. The list is generated based on a matrix that has been input, and contains 4 elements
#that are all needed in order to cache the inverse and check if it has been cached.

## Both functions can easily be run together by running code such as:
        ## A<-cbind(1:3,2:4,c(1,1,0)) # or whatever invertible matrix desired.
        ## L<-makeCacheMatrix(A)
        ## cacheSolve(L)

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This function takes in the output from the makeCacheMatrix function, and uses it to determine if getting the inverse of 
## the matrix is needed (if the matrix being inverted is new or has been changed), and if so, it gets it, and stores the 
## matrix for future use. Otherwise it outputs the previously cached inverse, and displays the message "getting cached data".

cacheSolve <- function(x, ...) {
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
