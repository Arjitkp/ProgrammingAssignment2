
## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) { # define the argument with default mode of "matrix"
  in_m <- NULL                     #initialize in_m as NULL; will hold value of matrix inverse 
  set <- function(y){    #define the set function to assign new
    x <<- y             # value of matrix in parent environment
    in_m <<- NULL    #if there is a new matrix, reset in_m to NULL
  } 
  
  get <- function() x     #define the get fucntion - returns value of the matrix argument
  
  set_in <- function(inverse) in_m <<-inverse     #assigns value of in_m in parent environment
  
  get_in<- function() in_m                      #gets the value of in_m where called
  
  list(set=set,get=get,set_in=set_in,get_in=get_in)
  

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

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
