## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix())
{
  ## Initialize inverse matrix
  i <- NULL
  
  ## Method to set the matrix
  set<-function( matrix )
  {
    x <<- matrix
    i <<- NULL
  }
  
  ## Method to get the matrix
  get<-function()
  {
    x
  }
  
  ## Method to set the inverse of the matrix
  setInverse<-function( inverse )
  {
    i <<- inverse
  }
  
  ## Method to get the inverse of the matrix
  getInverse<-function()
  {
    i
  }
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) 
{
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  
  ## Just return the inverse matrix if it already set.
  if(!is.null(i))
  {
    message("getting cached data")
    return(i)
  }
  
  ## get the matrix
  data <- x$get()
  
  ## Calculate the inverse of the matrix
  i <- solve(data) %*% data
  
  ## Set the inverse matrix
  x$setInverse(i)
  
  ## Return the inverse matrix
  i
}
