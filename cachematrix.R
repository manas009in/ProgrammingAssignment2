# This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
      xinv <- NULL # this is where the result of inversion is stored
      # A set function, use this to set a matrix to object created by makeCacheMatrix function
      set <- function(y) {
  x <<- y
  xinv <<- NULL # it also initialises xinv to null
      }
      get <- function() x # return the input matrix
      setInv <- function(inv) xinv <<- inv # set the inversed matrix
      getInv <- function() xinv # return the inversed matrix
      # return a list that contains these functions
      list( set = set, get = get,
       setInv = setInv,
       getInv = getInv )
  }
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
      m <- x$getInv() # get the inversed matrix from object x
      if(!is.null(m)) { # if the inversion result is there
  message("getting cached data")
  return(m) # return the calculated inversion
      }
      data <- x$get() # if not, we do x$get to get the matrix object
      m <- solve(data) # we solve it
      x$setInv(m) # we then set it to the object
      m # return the solved result
  }
