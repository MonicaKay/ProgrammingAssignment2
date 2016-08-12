# Overall, in this assignment, we are using lexical scoping rules to be able to cache a value so 
# that recalculating of a the result is not required as it can take a considerable amount of time
# for some of these calculations.

# The makeCacheMatrix() function initially sets i to NULL (a empty value).  
# The set() function within the mackCacheMatrix() function is assigning the values of y and NULL 
# to variables x and i in the parent environment.  The parent environment is used to implement lexical scoping.
# The get() function uses lexical scoping in that x is not defined within the function
# thus x is retreived from the parent environment.  This is the same with the setinverse()
# and getinverse() funtions, whereas i is coming from the parent environment.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# The cacheSolve() function looks to the makeCacheMatrix() function to see if the inverse has been calculated.
# This would be apparent if i != NULL.  If this is the case, the message "getting cashed inverse" 
# will be returned, along with the cashed value from the parent environment.
# If i is still NULL, the inverse of the matrix will be calculated with the solve() function and returned. 
# This calculated inverse is then stored in the parent environment.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached inverse")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

