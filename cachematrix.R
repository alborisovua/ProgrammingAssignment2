## R SCRIPT

## Matrix inversion is usually a costly computation 
## and there may be some benefit to caching the inverse 
## of a matrix rather than compute it repeatedly (there are 
## also alternatives to matrix inversion that we will not discuss here)

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i  <- NULL
  set  <- function(y){
    x <<- y
    i <<- NULL 
  }
  get  <- function() x
  setinverse  <- function(inverse) i  <<- inverse
  getinverse  <- function() i
  list(set= set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
  
}


## The following function inverses the matrix created with the above function. 
## However, it first checks to see if the matrix has already been inversed. 
## If so, it gets the inversed matrix from the cache and skips the inversion. 
## Otherwise, it inverses the matrix and sets the value in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  i  <- x$getinverse()
  if (!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data  <- x$get()
  i  <- solve(data, ...)
  x$setinverse(i)
  i
}


## END OF SCRIPT