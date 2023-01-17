makecachematrix <- function(x = matrix()) {
  
  inv <- NULL            #initializing inverse as NULL
  set <- function(y) {    
    x <<- y
    inv <<- NULL
  }                       #this sets the elements of the matrix   
  get <- function() x     # this gets the elements of the matrix
  setinverse <- function(inverse) inv <<- inverse  #Sets the elements of the matrix inverse
  getinverse <- function() inv   #Gets the elements of the matrix inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cachesolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {               
    message("getting cached data")
    return(inv)                           #returns cached data if inverse is not NULL  
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv                                     #retuns a matrix that is the inverse of matrix "x"
}
