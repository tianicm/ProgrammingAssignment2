## Josiane-Marie Stewart

## Programming Assignment 2: Caching the Inverse of a Matrix

## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly 
## The assignment is to write a pair of functions that cache the inverse of a matrix.

## The pair of functions to be created are the following:
## 1.makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## 2.cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

##To Test functions:
## my_matrix = makeCacheMatrix(matrix(c(3,4,5,6), nrow=2, ncol=2))
## my_matrix$get()         ## Returns original matrix
## cacheSolve(my_matrix)   ## Computes, caches, and returns matrix inverse
## my_matrix$getInverse()  ## Returns matrix inverse
## cacheSolve(my_matrix)   ## Returns cached matrix inverse using previously computed matrix inverse
## my_matrix$set(matrix(c(1,2,3,13,8,0,5,91,35), nrow=3, ncol=3)) # Modify existing matrix
## cacheSolve(my_matrix)   ## Computes, caches, and returns new matrix inverse
## my_matrix$get()         ## Returns matrix
## my_matrix$getInverse()  ## Returns matrix inverse

## The first function "makeCacheMatrix" creates a special "matrix", which
## is a list containing a function setting the value of the matrix, 
## getting the value of the matrix, setting the inverse of the matrix, 
## and at last, getting the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  ## set the inverse matrix
  setInverse <- function(inv) m <<- inv
  getInverse <- function() m
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This second function "cacheSolve" calculates the inverse of the special "matrix" 
## created with the above function. It first checks to see if the inverse of the matrix
## has already been calculated. If so, it gets the inverse from the cache and skips the
## computation. Otherwise, it calculates the inverse of the data and sets the inverse
## matrix in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  ## return the inverse matrix if it has been calculated already
  if (!is.null(m)) {
    message("Inverse matrix has been calculated already, getting cached data.")
    return(m)
  }
  
  matx <- x$get()
  ## calculate the inverse matrix
  m <- solve(matx, ...)
  ## store that inverse matrix in the cache
  x$setInverse(m)
  ## print matrix, thereby 'return' it
  m
}

