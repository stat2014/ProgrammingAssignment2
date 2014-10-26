### The following functions are used to calculate inverse of the matrix
###  using Cache. The functions should be used concurrently.
### If inverse of matrix is already available
### the function use the results without need to calculated again
## thereby saving time.  
 
 makeCacheMatrix <- function(m = matrix()) {
      # the argument m is matrix - should be square matrix
	  # i.e. invertable matrix 
      # initiatize s, which will be used to save catched data   
       s  <- NULL
	  # set value of matrix 
         set  <- function(y){
               m <<- y
	  # use of function <<- to assign value to an environment 
	  # different than current one 
               s <<- NULL 
         }	   
	  # get value of the matrix 
         get  <- function() m
		 
      # set inverse of the matrix 
         set_inverse  <- function(inverse) s  <<- inverse
     # get inverse of the matrix 
         get_inverse  <- function() s

    # list of the values 
       mlist <- list(set= set, get = get, set_inverse = set_inverse, 
               get_inverse = get_inverse)

    # return the values 
		 return(mlist)
  }
 
 # the following function does inverse of the matrix 
 # will check if the cached data is available.
 # if available with return it, if not use the function 
 # solve to get one and return it. 
 
 cacheSolve <- function(m, ...) {
      # argument m is returned value from previous makeCacheMatrix function 
      # get inverse of the matrix 
         s  <- m$get_inverse()
    # check if the matrix is available
    # if available return it 
     if (!is.null(s)){
                 message("cached data found: returning the cached data")
                 return(s)
         } else{
		        message("no chached data found: calculating inverse of the matrix")
    # else, get the inverse of the matrix
               data  <- m$get()
               s  <- solve(data, ...)
    # set inverse of the matrix 
               m$set_inverse(s)
               return(s)
 }
 }
 
 ### the following is testing, implementation 
 ### please remove comment marks for testing 
 
 #set.seed(1234)
 #Xmat1 = matrix(round(runif(1000000,0,1),1), nrow=1000, ncol=1000)
 #t1 = makeCacheMatrix(Xmat1)
 #out1 <- cacheSolve(t1)
 # if we run again on t1
 # it will be quicker as this will used cache data
 #out2 <- cacheSolve(t1)

# however for completely new matrix, same time will be needed 
#set.seed(12345) 
#Xmat2 = matrix(round(runif(1000000,0,1),1), nrow=1000, ncol=1000)
#t2 = makeCacheMatrix(Xmat2)
#out3 <- cacheSolve(t2)
