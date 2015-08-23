## My code caches the matrix inverse so that an already calculated inverse doesn't need to be recalculated
## I essentially used the vestor example code to base my code bc after trying various ways to make it work I 
## found that to be the simplest

## list containing function to set & get value of Matrix and Inverse Matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y)
  {
    x <<- y
    m <<- NULL
  }
  get <- function() x ## returns the value
  setInvMatrix <- function(invMatrix) m <<-invMatrix ## sets the m value to the inverse matrix
  getInvMatrix <- function () m ## returns the m value which is the Inverse Matrix
  list(set= set, get=get, setInvMatrix = setInvMatrix,getInvMatrix = getInvMatrix) ## allows the function to be returned as a list so you can access each method

}


## retrieves the inverse if it is already cached if not then it calculates the inverse
cacheSolve <- function(x, ...) {
  m <- x$getInvMatrix()
  if(!is.null(m)) {   ## returns the inv matrix if it has been calculated already
    message("retrieving")
    return(m)
  }
  im <- x$get() 
  m <- solve(im) ## find inverse
  x$setInvMatrix(m) ## sets the inverse so it can be recalled later on if needed
  m ## print in console when method is called 
}

