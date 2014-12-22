## This file contains a pair of functions that cache the inverse of a matrix.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
    outInvMatrix <- NULL
  
    set <- function(y) {
        x <<- y
        outInvMatrix <<- NULL
    }
  
    get <- function() x
  
    setInvMatrix <- function(solve) outInvMatrix <<- solve
  
    getInvMatrix <- function() outInvMatrix
  
    # return a list of functions as an R object
    list(set=set, get=get, 
         setInvMatrix=setInvMatrix, 
         getInvMatrix=getInvMatrix)
  
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## above. If the inverse has already been calculated (and the matrix has not changed), then 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    outInvMatrix <- x$getInvMatrix()
  
    if(!is.null(outInvMatrix)) {
        message("getting cached data")
        return(outInvMatrix)
    }
  
    # matrix inverse not cached so create inverse and cache.
    data <- x$get() 

    # create solved matrix
    outInvMatrix <- solve(data) 

    # cache solved matrix
    x$setInvMatrix(outInvMatrix) 
  
  outInvMatrix

}
