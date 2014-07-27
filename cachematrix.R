
###############################################################################
##makeCacheMatrix - Creates a specialized matrix object that caches its inverse
##cacheSolve - Computes the inverse of the matrix object returned by the 
##makeCacheMatrix function.It conditionally retrieves the inverse of the matrix
##from the cache and recomputes the inverse if the matrix has changed
###############################################################################




##Creates a specialized matrix object that caches its inverse that takes the 
##following arguments - 
##x - The matrix to be inverted.The function assumes that the supplied matrix is
##invertible.
makeCacheMatrix <- function(x=matrix()) {
        
  inverse <- NULL
  
  set <- function(y) {
          
         x <<- y

         ##Invalidates the cache whenever the matrix is modified
         inverse <<- NULL
          
  }
  
  get <- function() x
  
  setinverse <- function(inv) inverse <<- inv
  
  getinverse <- function() inverse
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
        
}



##Computes the inverse of a matrix that takes the following arguments:-
#x - Specialized matrix object returned by the makeCacheMatrix method
## The function uses the solve method to compute the inverse of a matrix
cacheSolve <- function(x,...) {
        
  inverse <- x$getinverse()
  
  ##Checks whether the cache contains the inverse of the matrix
  if(!is.null(inverse)) {
         message("getting cached data") 
         return(inverse)
  }
  
  data <- x$get()
  
  inverse <- solve(data,...)
  
  x$setinverse(inverse)
  
  inverse
        
}



