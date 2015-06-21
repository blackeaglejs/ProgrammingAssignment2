## Matrix inversion is a computationally costly function. 
## This set of functions takes a square matrix and computes its inverse.
## That inverse is then cached and be called for later use. 


## This function caches the matrix and sets retrieving the cache.
## It returns a list of four functions.

makeCacheMatrix <- function(x = matrix()) {
      ## invm is the inverse of the matrix. since it hasn't been 
      ## calculated yet, it is set to NULL. 
      invm <- NULL
      
      ## this function picks the matrix to be inverted. if a new 
      ## matrix is loaded, the inverse is cleared. 
      set <- function(y) {
            x <<- y
            invm <<- NULL
      }
      
      ## retrieves the value of the matrix.
      get <- function() x
      
      ## sets the inverse of the matrix. 
      setinverse <- function(inverse) a <<- solve(x)
      
      ## this next line returns the value of the matrix just inverted.
      getinverse <- function() a
      
      ## returns a list of functions.
      list(set = set,
           get = get
           setinverse = setinverse
           getinverse = getinverse)
}


## This function returns the value of a cached matrix if one exists.
## If one does not exist, it calculates the inverse and saves it.

cacheSolve <- function(x, ...) {
      ## Gets the value of the inverted matrix from the other function.
      b <- x$getinverse
      
      ## this if statement checks to see if b is null. 
      ## b would be null if there is no cached matrix. 
      ## if b is NOT null, it returns that value and ends the function.
      if(!is.null(b)) {
            message("getting cached data")
            return(b)
      }
      
      ## At this point, the inverse matrix doesn't exist. 
      ## This grabs the argument that was passed in the function
      ## It then inverts and stores the value. 
      data <- x$get()
      b <- solve(data, ...)
      x$setinverse(b)
      b
}
