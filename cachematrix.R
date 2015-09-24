## The aim of this two functions is to avoid repeating the computation of the inverse
## of a matrix in case we have just computed it. The first function is the function we have to 
## use to define the matrix, it does not compute anything. The second one, is the one that checks
## if the inverse has already been computed, and if not, it computes it and stores it in the cache.


## makeCacheMatrix is a function that takes as an input a matrix, and defines
##four functions that act on this input:
##   -set: changes the value of the input matrix
##   -get: gives you as an output the matrix "stored" in makeCacheMatrix
##   -setinverse: changes the value of the parameter i (it will be the inverse of the matrix when
##    we will use the cacheSolve function)
##    getinverse: gives you as an output the parameter i
##    Notice that the parameter i is set to NULL at the beginning of the function makeCacheMatrix,
##    this is because when you define a new matrix, the inverse is not known a priori.

makeCacheMatrix <- function(x = matrix()) {
      i<-NULL
      set <- function(y){
            x<<-y
            i<<-NULL
      }
      get <- function() x
      setinverse <- function(inverse) i<<-inverse
      getinverse <- function() i
      
      list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve is a function that, using three of the four functions defined inside makeCacheMatrix,
## checks if the inverse of the matrix we want to compute has already been computed before, and,
## if this is the case it tell us in a message "getting cache data" and then give us the inverse.
## If the inverse has not been previously computed, the matrix "stored" in makeCacheMatrix is 
## recovered and its inverse is computed by the "Solve" function. Notice how this value of 
## the inverse is "stored" in MakeCacheMatrix by the setinverse function. This is necessary to 
## be able to recover it from the cache in case we need to compute it again.


cacheSolve <- function(x, ...) {
      i <- x$getinverse()
      if(!is.null(i)){
            message("getting cached data")
            return(i)
      }
      data <- x$get()
      i <- solve(data)
      x$setinverse(i)
      i
}
