# R Programming Assignment 2

# A pair of functions to cache the inverse of a matrix
# Assume for now that the supplied matrix is always invertible

# This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) { # use this to change the matrix
      x <<- y # changes x in the makeCacheMatrix function too
      inv <<- NULL # resets inv as this will change when matrix is changed
    }
    get <- function() x # requires no input; retrieves the matrix to be inverted
    setinv <- function(i) inv <<- i # use to set inverse separately (within makeCacheMatrix function too)
    getinv <- function() inv # retrieves cached matrix inverse
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv) # list of all functions so that they can be retrieved in next function
  }



# This function computes the inverse of the matrix returned above
# Retrieves cached inverse if the inverse has already been calculated and matrix has not been changed
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

    inv <- x$getinv() # retrieves cached inverse
    if(!is.null(inv)) { # checks whether cached inverse exists
      message("getting cached data")
      return(inv) # returns cached inverse
    }
    # if no cached value..
    data <- x$get() # retrieves matrix from makeCacheMatrix function
    inv <- solve(data, ...) # calculates inverse of matrix
    x$setinv(inv) # sets to calculated inverse of matrix
    inv # displays inverted matrix
  
}
