
###################################################################
######################## SHREEYA PATEL : START ####################
###################################################################

### Matrix inversion is usually a costly computation and there may be some benefit 
### to caching the inverse of a matrix rather than compute it repeatedly 


## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

### functionName : makeCacheMatrix 
### input x <- matrix
### output is a cached of the Matrix inverse

### Performs the following 
# 1 set the value of the Matrix
# 2 get the value of the Matrix
# 3 set the value of the Inverse
# 4 get the value of the Inverse

makeCacheMatrix <- function(x = matrix()) {
  
  matrixI <- NULL
  set <- function(y) {
    x <<- y
    matrixI <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) matrixI <<- inverse
  getinverse <- function() matrixI
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


### cacheSolve: This function computes the inverse of the special "matrix" 
### returned by makeCacheMatrix above. If the inverse has already been calculated 
### (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

##### Return a matrix that is the inverse of 'x'

### functionName : cacheSolve 
### input x Matrix
### output is a cached of the Matrixinverse to be returned from Cache if it is already calculated


cacheSolve <- function(x, ...) {
        
  
  matrixI <- x$getinverse()
  if (!is.null(matrixI)) {
    message("Getting cached Matrix Data ")
    return(matrixI)
  }
  data <- x$get()
  matrixI <- solve(data, ...)
  x$setinverse(matrixI)
  matrixI
}

###################################################################
######################## SHREEYA PATEL :  END #####################
###################################################################
