## RProgramming Assignment 2 - Cache Matrix
## These functions provide a new matrix type that stores (caches) the inverse of the matrix so that
## this value does not have to be recomputed while the values of the matrix remain the same

## Example usage:
## m <- matrix(c(1, 1, 1, 3, 4, 3, 3, 3, 4), 3)
##
## cache.matrix <- makeCacheMatrix(m)
##
## cacheSolve(cache.matrix)
## Outputs:      
##       [,1] [,2] [,3]
## [1,]    7   -3   -3
## [2,]   -1    1    0
## [3,]   -1    0    1
##
## cache.matrix$getinverse()
## Outputs:      
##       [,1] [,2] [,3]
## [1,]    7   -3   -3
## [2,]   -1    1    0
## [3,]   -1    0    1
##
## cacheSolve(cache.matrix)
## Outputs:
## getting cached data
##       [,1] [,2] [,3]
## [1,]    7   -3   -3
## [2,]   -1    1    0
## [3,]   -1    0    1
##
## m2 <- matrix(c(0, 1, -3, -3, -4, 4, -2, -2, 1), 3)
##
## cache.matrix$set(m2)
##
## cache.matrix$getinverse()
## Outputs: 
## NULL
##
## cacheSolve(cache.matrix)
## Outputs:      
##       [,1] [,2] [,3]
## [1,]    4   -5   -2
## [2,]    5   -6   -2
## [3,]   -8    9    3



## This function creates the cache matrix object that will cache its inverse

makeCacheMatrix <- function(matrix.value = matrix()) {
  # Sets the matrix inverse to null
  matrix.inverse <- NULL
  
  # Returns the value of the matrix
  get <- function() { 
    matrix.value
  }
  
  # Sets new value for cache matrix and clears the inverse
  set <- function(new.value) {
    matrix.value <<- new.value
    matrix.inverse <<- NULL
  }
  
  # Returns the value of the inverse of the cache matrix
  getinverse <- function() {
    matrix.inverse
  }
  
  # Sets the value of the inverse of the cache matrix
  setinverse <- function(inverse.value) {
    matrix.inverse <<- inverse.value
  }
  
  # Allow functions to be accessed by subsetting (i.e. $ sign)
  list(
    get = get,
    set = set,
    getinverse = getinverse,
    setinverse = setinverse
  )
}


## This function calculates the inverse of the cache matrix - either by using the cached value if
## the matrix is unchanged, or recomputing the inverse if the matrix has changed

cacheSolve <- function(cache.matrix, ...) {
  # Gets the inverse value currently stored in the cache matrix
  matrix.inverse <- cache.matrix$getinverse()
  
  # Returns cached inverse value if it exists, and outputs a message
  # (this value will exist if the matrix is unchanged since cacheSolve was last run)
  if(!is.null(matrix.inverse)) {
    message("getting cached data")
    return(matrix.inverse)
  }
  
  # Gets the value of the underlying matrix
  matrix.value <- cache.matrix$get()
  
  # Solves to find a matrix that is the inverse of the underlying matrix of 'cache.matrix'
  matrix.inverse <- solve(matrix.value, ...)
  
  # Sets the value of the inverse in the cache matrix
  cache.matrix$setinverse(matrix.inverse)
  
  # Returns a matrix that is the inverse of 'cache.matrix'
  matrix.inverse
}
  