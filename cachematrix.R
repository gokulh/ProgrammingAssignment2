##This function takes a matrix object and returns a list of functions (set, get, setinverse, getinverse).
##These functions functions allows manipulation of matrix object.
## The <<- operator helps in maintaining state of the variable beyond the current environment/run.
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
##This function cacheSolve takes an object returned by the makeCacheMatrix function and computes the inverse.
##This is done by using the solve function.
##If the inverse of the matrix was not previosly computed then the inverse is computed the first time. 
##It also updates the cache to hold values for subsequent requests.
##if the inverse of the matrix in question exists, it returns the cached inverse and does not compute.
cacheSolve <- function(x, ...) {
  
  i = x$getinverse() #if the inv is already calculate get it.
  
  #Check if the inv value from above line is not NULL. then return the value from above line.
  if (!is.null(i)){
    # get it from the cache and skips the computation. 
    message("getting cached data")
    return(i)
  }
  
  #Calculate the inverse using the solve fuction.
  mymatrix.data = x$get()
  i = solve(mymatrix.data, ...)
  
  #set the value of i in cache. So it does not have to calculate again.
  x$setinverse(i)
  
  #return the inv matrix
  return(i)
}
