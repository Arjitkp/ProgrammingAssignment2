
## The functions calculates the inverse of a gien invertible matrix.

makeCacheMatrix <- function(x = matrix()) {
  in_m <- NULL   
  set <- function(y){    #sets another matrix the the value of X and refreshes the value of inverse
    x <<- y
    in_m <<- NULL    #in_m is the inverse of the given matrix
  } 
  
  get <- function() x     #getter function to get the value of c
  
  set_in <- function(inverse) in_m <<-inverse     #fixes the inverse of matrix
  
  get_in<- function() in_m
  
  list(set=set,get=get,set_in=set_in,get_in=get_in)
  

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  in_m <- x$get_in()
  if(!is.null(in_m)){        #checks for NULL
    message("getting cached data")
    return(in_m)
  }
  data<- x$get()
  
  in_m<-solve(data)    #calculates for inverse
  
  x$set_in(in_m)
  
  in_m
}
