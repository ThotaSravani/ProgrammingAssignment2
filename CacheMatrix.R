# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(a = matrix()) {
  inv <- NULL
  set <- function(b) {
    a <<- b
    inv <<- NULL
  }
  get <- function() a
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

# This function assumes that the matrix is always invertible.
cacheSolve <- function(a, ...) {
  inv <- a$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- a$get()
  inv <- solve(data)
  a$setinverse(inv)
  inv
}

# Sample Run 
# To create an matrix
# set the values matrix
# m<- matrix(1:4,nrow=2)

# m1 <- makeCacheMatrix(m)
# To get the data
# m1$get()
#       [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# No cache in  the first run 
# cacheSolve(m1)
#      [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
# Retriving from the cache in the second run
# cacheSolve(m1)
#      [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
