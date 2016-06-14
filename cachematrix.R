## The aim of the functions are to cache potentially time-consuming inverse computations of a matrix.
## If the contents of a matrix are not changing, we cache the value of the inverse so that when we need it again,
## it can be looked up in the cache rather than recomputing it.

## The first function, makeCacheMatrix is really a list containing a function to

##set the value of the matrix
##get the value of the matrix
##set the value of the inverse
##Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {     ##sets the value of a matrix
    x <<- y                ##Super Assigment Operator to assign the value to x in the global enviroment.
    inverse <<- NULL
  }
  get <- function() x          ##returns the matrix value
  setinverse <- function(i) inverse <<- i  ##stores the inverse once it is computed and this function is called.
  getinverse <- function() inverse          ## returns the cahed value of inverse if the computed value exists
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## The following function calculates the inverse of the matrix created with the above function.
## However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		
		
  inverse <- x$getinverse()
  if(!is.null(inverse)) {           ##checks if the inverse already exists in the cache
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()                    ##gets the value of the matrix         
  inverse <- solve(data)             ## Calculates the inverse if it doesn't exist in the cache       
  x$setinverse(inverse)
  inverse
}




