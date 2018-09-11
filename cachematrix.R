## makeCacheMatrix is a class which has matrix and 4 functions to set, get, setinverse and getinverse function.
## Set - set matrix
## Get - get matrix
## setinverse - calculate inverse of the matrix
## getinverse - get the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
  get <- function() x
  setinverse <- function(inverse) i <<- solve(inverse)
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse  )
}

## cacheSolve function return the inverse of the given matrix. 
## First it checks if inverse of the matrix is already available in cache and if it is available it return from there.
## otherwise it calculate the inverse by calling setinverse function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
  
    if(!is.null(i)){
    message("getting inverse from cache")
    return(i)    
    }
    
    data <- x$get()
    ##  i <- solve(data, ...)
    ##  x$setinverse(i)
    i <- x$setinverse(data)
    return(i)
}
