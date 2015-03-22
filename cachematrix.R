## The following two functions are designed to save time for a user 
## when they need to calculate the inverse of matrices, by storing previously
## calculated inverses in the system's cache and accessing them as required. 

## Function 1: The first function calculates the inverse of a chosen matrix
## and stores the result in the system's cache. It also sets up a list 
## to aid a second function in figuring out whether a result already exists 
## in the cache, to save that function processing time. See each step's 
## comments for further details.

makeCacheMatrix <- function(x = matrix()) {


     # Load the MASS library so ginv() can be used, which has more 
     # functionality since solve() only works on square matrices.

           library(MASS)


     # Initialise an initial "matr" variable to NULL, which ensures there is
     # no left over value from previous computations.

           matr   <- NULL


     # Set the matrix: Pass the matrix argument into "makeCacheMatrix" so it
     # is in the cache environment.

        set  <- function(y) {
               x <<- y
            matr <<- NULL
         }

     # Assigns "get" function to the makeCacheMatrix environment

          get  <- function() x

     # Take a value "inverse" and sets it within "makeCacheMatrix"

       setinverse <- function(ginv) matr <<- inverse

     # Checks to see if an inverse already exists. If it doesn't 
     # (ie: it's NULL), it calculates and assigns it to "matr" within the
     # cache.

         getinverse <- function() {
           if(is.null(matr)){
             matx <<- ginv(x)
         }
          return (x)

     # Stores everything on a list so they are accessable
     # in character formats in the "cacheSolve" function.
           
       list(get=get, set=set, setinverse=setinverse, getinverse=getinverse)

}


## Function 2: This function checks to see if a inverse matrix value  
## already exists in the cache. If it exists, it uses the existing  
## value to save processing time. Otherwise, it calculates the  
## inverse of the matrix as normal. See each step's comments for
## further details. 


cacheSolve <- function(x, ...) {


     # Assigns the "matr" value to the local environment

           local_matr <- x$getinverse() 

     
     # If "matr" does exist, it gets the value from the cache and returns
     # the value, saving processing time.      

     if(!is.null(local_matr)) {
           message("getting cache data")
 	     return(local_matr)
           
           } else {

     # If a value doesn't already exist, it gets the data for the matrix and
     # calculates its inverse before returning the value.

           data <- x$get()
           local_matrix <- ginv(data,...)
           local_matrix$setmean(matr)
           
         return(local_matrix)
           }
}