## To avoid repeated computations of inverting a matrix, the functions below
## will cache the inverse and cache the matrix and the other one will 
## compute the inverse (get it from the cache if it exists, or compute the 
## inverse and set the cache in the makeCacheMatrix)
## functions do

## This function create the cache for the matrix passed to it

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  #Calculate the inverse of the matrix
  setinverse <- function(solve) i <<-solve
  getinverse <- function() i
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


## The below function computes the inverse of the matrix returned by 
## makeCacheMatrix. Note that the below functions assumes that the matrix is
## invertible

cacheSolve <- function(x, ...) {
  i <-x$getInverse
  if(!is.null(i)) {#If the there is a cached inverse, return it
    return(i)
  }
  
  #If no cache exist, compute the inverse
  matr <- x$get()
  i <- solve(matr,...)
  #Set the inverse in the cache matrix
  x$setinverse(i)
  #Return the inverse
  i
  
}


myMatrix <- matrix(c(1,2,4,1), nrow = 2,ncol = 2)

mat <- makeCacheMatrix(myMatrix)

#Call the cacheSolve function while passing the matrix object
inv <- cacheSolve(mat)
