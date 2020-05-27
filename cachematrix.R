## These two functions work together to create inverse of a matrix
##and store in it in the cache so that repeatedly calculating inverse
##of same matrix can be avoided.


## makeCacheMatrix creates a set of functions used by cacheSolve 
## to set or get inverse matrix in cache

makeCacheMatrix <- function(x = matrix()) {
  
  #initialise cache to null
  
  cache<-NULL
  
  #create the matrix in working environment
  
  set<-function(y)
  {
    x<-y
    cache<<-NULL
  }
  
  #return the value of matrix in working environment
  get<-function() x
  
  #invert the matrix and store in cache
  setinverse<-function(inverse) cache <<- inverse
  
  #return the inverse of matrix from cache
  getinverse<-function() cache
  
  #return the created functions to working environment
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  
  
  
  
}


## If the inverted matrix doesnt exist in cache, it is created in working environment
##and its inverted value is stored in cache

cacheSolve <- function(x, ...) {
  
  ##trying to get inverse of matrix from cache
  cache<-x$getinverse()
  
  if (!is.null(cache)) {
    message("getting cached data")
    
    #display inverse in console
    
    return(cache)
    
  }
  
  #create the matrix since it doesnt exist
  
  matrix<-x$get()
  
  #set and return inverse of matrix
  cache<-solve(matrix,...)
  
  
    #set inverted matrix in cache
    
    x$setinverse(cache)
    
  
  
  return(cache)
}

