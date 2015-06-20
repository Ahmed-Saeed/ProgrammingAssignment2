## Functions that create a matrix that can cache its inverse 
## and use it to get matrix invers 

## Function that create a matirx with cachable inverse 

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(i) inverse <<- i
  
  getInverse <- function() inverse
  
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Using the new CacheMatrix to get matrix inverse

cacheSolve <- function(x, ...) {
        i <- x$getInverse()
        
        if(!is.null(i)){
          message("getting cached inverse")
          return(i)
        }
        
        matrix <- x$get()
        i <- solve(matrix)
        x$setInverse(i)
        i
}
