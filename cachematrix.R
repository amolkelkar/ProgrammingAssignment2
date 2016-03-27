## the below function makeCacheMatrix creates a vetor of four functions get, set,
## getInverse and setInverse. Another function 'cacheSolve' is function which decides
## if the defined vector has a inverse set. If the inverse is present it will retrieve
## it, else it will calculate the inverse itself, store back to the vec and display
## the inverse

makeCacheMatrix <- function(x = matrix()) {
InV <- NULL                               
  set <- function(y) {          
    x <<- y
    InV <<- NULL
  }
  get <- function() x
  setInverse <- function(Inverse) InV <<- Inverse
  getInverse <- function() InV
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}

cacheSolve <- function(x, ...) {
            ## Return a matrix that is the inverse of 'x'
    ## see if the inverse has already been cashed
    inv <- x$getInverse()
    if(!is.null(inv)){
        message("Getting cached data")
        return(inv)
    }
    ## if inverse is not present calculate inverse
    message("Calculating matrix inverse!!")
    mat <- x$get()
    inv <- solve(mat, ...)
    ## save the calculated inv in the created vector x and display the output
    x$setInverse(inv)
    inv
}
