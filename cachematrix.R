## makeCacheMatrix creates an object that contains functions to 
## set the value of the matrix, get the value of the matrix,
## set the inverse of the matrix, and get the inverse of the matrix.


## cacheSolve checks if the inverse of the matrix has been calculated. 
## If so, fetch it from the cache, otherwise calculate the inverse
## and store it into the cache. Then return the inverse of the matrix.



## Creates a special matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL # initialize inverse to null
  set <- function(y){ # sets the value of the matrix
    x <<-y
    inv <<- NULL
  }
  get <- function() x # gets the value of the matrix
  setinv <- function(v) inv <<- v # sets the inverse of the matrix
  getinv <- function() inv # gets the inverse of the matrix
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)  
}



## Computes the inverse of the special matrix. If inverse
## has already been calculated, then retrieve inverse from cache. 
cacheSolve <- function(x, ...) {
    inv <- x$getinv() # Gets the inverse of the object
    if(!is.null(inv)){ # If previously calculated, retrive from cache
      message("getting cached data")
      return(inv)
    }
    data <- x$get() # Otherwise get the data
    inv <- solve(data, ...) # Calculate inverse of the matrix
    x$setinv(inv) # Set the inverse into cache 
    inv
}
