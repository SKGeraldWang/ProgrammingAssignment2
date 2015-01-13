## These two functions provide ability to cache solution of inversing a matrix. 
## If the contents of a matrix are not changing, it can be looked up in the cache rather than recomputed the inverse.
## This is beneficial when a matric is needed to be inversed multiple times
 
## The function of makeCacheMatrix creates a special "matrix", which is a list containing functions as follows.
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse 

 makeCacheMatrix <- function(x = matrix()) {
  
        s <- NULL # initialize a solution NULL

        # set a special Matrix 
        set <- function (y) {
                x <<- y
                s <<- NULL
        } 

        # get the actual Matrix from the special Matrix
        get <- function() x
       
        # set the inverse of the Matrix
        setInv <- function(s) s <<- s
                   
        # get the inverse of the Matrix
        getInv <- function() s
                        
        # return a list of functions as the special vector
        list(set = set, get = get, setInv = setInv, getInv = getInv)
}

## The following function calculates the inverse of the special "matrix" created with the above function. 
# It first checks to see if the inverse has already been calculated. 
# If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the data and sets the solution in the cache via the setInv function.

cacheSolve <- function(x, ...) {
         ## Return a matrix that is the inverse of 'x'
        
         s <- x$getInv() # try to get a inverse
         
         # check if the inverse exsit, if so return it
         if (!is.null(s)){
                 message("getting cached data")
                 return(s) # skp the following caculations
         }
         
         # if the inverse does not exsit
         s <- solve(x$get()) # caculate the inverse
         
         x$setInv(s) # cache the solution
         
         s # return the solution
 }
