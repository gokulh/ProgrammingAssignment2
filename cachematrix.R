## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL

    #set the matrix
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    
    #get the matrix
    get <- function() { 
      x 
    }
    
    #sets the inverse of a matrix
    setinverse <- function(inv) { 
      i <<- inv 
    }
    
    #return the inverse of a matrix
    getinverse <- function() { 
      i 
    }
  
    #returns a list of functions
  list(set = set, get = get, getinverse = getinverse, setinverse =  setinverse)
  
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  
  inv = x$getinv() #if the inv is already calculate get it.
  
  #Check if the inv value from above line is not NULL. then return the value from above line.
  if (!is.null(inv)){
    # get it from the cache and skips the computation. 
    message("getting cached data")
    return(inv)
  }
  
  #Calculate the inverse using the solve fuction.
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  #set the value of inv in cache. So it does not have to calculate again.
  x$setinv(inv)
  
  #return the inv matrix
  return(inv)
}
