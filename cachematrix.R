## These functions implement a matrix type that caches its inverse to improve performance
##
## To test the correctness of this implementation, you can use the following steps
##
## ## Generate a random 3x3  Matrix:
## m <- matrix(sample(1:100,9,replace=TRUE),3,3)
## m
##
## ## invoke makeCacheMatrix to create an matrix object that caches its inverse
## cm <- makeCacheMatrix(m)
##
## ## compute the inverse
## inv <- cacheSolve(cm)
## inv
##
## ## To verify that the inverse is correct, multiply m X inv, you
## ## should get a matrix very close to the 3x3 identity matrix (1s in the diagonal from top left to 
## ## bottom right, 0s elsewhere, it won't be exact because of rounding)
## m %*% inv
##
## ## To verify that the inverse is cached, compute the inverse again
## ## You should see the message "returning cached inverse" .
## ## The matrix inv2 should be the same as the inv matrix returned
## ## previously
## inv2 <- cacheSolve(cm)
## inv2




## This function takes a matrix argument and returns an object (a list of functions)
## with operations for setting the value, retrieving the value, setting the inverse and rerieving the inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  
  setinverse <- function(inv)  inverse <<- inv
  
  getinverse <- function() inverse
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## This function takes an object constructed by the makeCacheMatrix function
## above, and returns the inverse of the data matrix.  If the inverse has been 
## computed previously, it will return the cached value rather than re-compute 
## the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("returning cached inverse")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  
  inv
}
