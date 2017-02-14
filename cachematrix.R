## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  inverse <<- NULL
  set <- function(y){
    x <<- y
  }
  get<- function() x
  setInverse <- function(inv_matrix = matrix()){
    if(identical(inv_matrix, solve(x)))
    inverse <<- inv_matrix
    else{
      message("It doesn't match inverse matrix(your input) and calculated matrix(in the cache) ")
    }
  }
  
  getInverse <- function() inverse
  
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
  
}


## Write a short comment describing this function

## 1. If there has inverse matrix in the cache, get that matrix.
## 2. If not, calculate inverse matrix by using solve() and then write cache by using setInverse().
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("getting cached inverse matrix.")
        return(inverse)
    }
    
    else{
      m <- x$get()
      inverse <- solve(m)
      x$setInverse(inverse)
      return(inverse) 
    }  
}

    
    
    
## Use this example
## m = matrix(1:4, 2, 2)
## a = makeCachematrix(m)
## > a$get()
##     [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > a$setInverse(m)
##  It doesn't match inverse matrix(your input) and calculated matrix(in the cache) 
##  (If you use setInverse(), you have to right input inverse matrix)
## > cacheSolve(a)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

##
## > cacheSolve(a)
## getting cached inverse matrix.
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##
