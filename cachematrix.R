## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# First, I am initializing i as an object within the function to be used later
# Second, I am setting the environment (according to the global envronment hierarchy) and takes an argument named y
#    - also, <<- form of the assignment operator assigns the value on the right side to an object in the parent environment
#    - also, I am setting i to NULL in the parent environment to to clear any value of i that had been cached by the prior execution 
# Third, I am defining the get function for vector x (due to lexical scoping, since x is not defined within get(), R retrieves it from parent environment)
# Fourth, I am defining the setter for the inverse
#    - also, since i is defined in the parent environment and need to access it after the inverse, use the <<- to assign the input argument to the value of i
# Fifth, I am defining the getter for the inverse
#    - also, R is using lexical scoping to find i to get its value
# Lastly, assigning each function to an element within a list, and return it to the parent environment
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
# Begins with a single argument and an ellipsis to allow additional arguments into the function
# Then, retrieve the inverse from the object passed in as the argument, calling getinverse onto i
# Next, if the new matrix is not NULL, then we have an inverted matrix and can return it to the parent environment
# Finally, returns a matrix to the parent environment by printing it
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
