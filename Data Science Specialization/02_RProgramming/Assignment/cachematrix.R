## Overall, the two functions work together to return the inverse of a matrix
## User can pass an invertible matrix to makeCacheMatrix 
##and it will return a list of functions
## cacheSolve uses this list. If the inverted matrix has been calculated before
## it will return that, else it will calculate and return the inverted matrix


## The function makeCacheMatrix creates a list of 4 functions 
##which perform 4 things:
# Set the matrix
# return the matrix
# Set the inverted matrix 
# return the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The function cacheSolve takes the aforementioned list as an argument and
## return the following:
# if the inverse of the matrix has been 'set' previously, it uses the 
#getinverse function to get that inverse matrix and returns it

# if the inverse of the matrix is not set, getinverse returns null and then 
#cacheSolve will calculate the inverse, set it in the makeCacheMatrix list
#and return it


cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
