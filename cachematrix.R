## 
##  Functions calculate matrix inversion. First function caches matrix inversion and second function 
##  first checks if inversion exists. Unless it doesn't function then calculates it.
## 


## function can compute inversion of matrix and cache this inverse

makeCacheMatrix <- function(x = matrix())  {

 m<-NULL
  set<-function(y){
  x<<-y
  m<<-NULL
}
get<-function() x
read_matrix<-function() m
inverse<-function(solve) m<<- solve
list(set=set, get=get, inverse=inverse, read_matrix=read_matrix)
return(inverse)
}


## checks if matrix inversion exists and calculates inverse if it does not
## find it.

cacheSolve <- function(x, ...) {                 
  matrixinverse <- x$getinverse()
  
  #if matrix inversion already exists, it is returned
  if(!is.null(matrixinverse)) {                 
    message("getting cached data - Inverse of the matrix")
    return(matrixinverse)
  }
  #if the inverse is not found it will be calculated
  read_data <- x$get()                               
  calculus <- solve(read_data, ...)
  x$setinverse(matrixinverse)
  return(calculus)
}
