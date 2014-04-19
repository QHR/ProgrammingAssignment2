#######################################################################
## The following pair of functions can be used to return the inverse  
## of a matrix such that it is (re)computed only if the matrix has  
## changed; otherwise a cached inverse is returned.
######################################################################


## The first function creates a list of functions that set local and
## global values of variables, both to save a cached value and 
## to determine whether a cached inverse value is stored.


makeCacheMatrix <- function(x = matrix()) {
 
    m <- NULL
    
    ## Element 1 of the list:
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    
    ## Element 2 of the list:
    get <- function() x
    
    ## Element 3 of the list:
    setinv <- function(inv) m <<- inv
    
    ## Element 4 of the list:
    getinv <- function() m
    
    ## The output of makeCacheMatrix is a list: 
    
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    
    ## NOTE: If you run makeCacheMatrix in the console, you can see each  
    ## element's value (a function) and the environment in which it exists.
  
}


## The second function returns the inverse of the matrix.
## If there's already an inverse saved, the cached inverse is returned; 
## otherwise, the new inverse is computed, stored, then returned as output.


## The input into cacheSolve should be the list from makeCacheMatrix

cacheSolve <- function(x, ...) {
  
  ## m is assigned to be the "getinv" element from the makeCacheMatrix list.
  
  m <- x$getinv()
  
  ## If m is null there is new data to be inverted (below);
  ## if m is not null, it equals the cached inverse, and will be returned. 
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## If m was null, we take the new matrix, which was stored
  ## in the "get" element of the makeCacheMatrix list,...
  data <- x$get()
  
  ##... and invert it.
  m <- solve(data, ...)
  
  ## Then we store this inverse as the cached value...
  x$setinv(m)
  
  ## and return the inverse as the output of the function
  m
  
}

######### TEST THE PAIR OF FUNCITONS ################################
## Try running the following:

## cacheSolve(z<-makeCacheMatrix(x<-matrix(c(1,2,3,2,3,1,4,4,4),3,3)))

## The function returns:            
##            [,1]       [,2]        [,3]
## [1,] -0.6666667  0.3333333  0.33333333
## [2,] -0.3333333  0.6666667 -0.33333333
## [3,]  0.5833333 -0.4166667  0.08333333

## Then run: 

## cacheSolve(z)

## The function returns: 
## getting cached data
##            [,1]       [,2]        [,3]
## [1,] -0.6666667  0.3333333  0.33333333
## [2,] -0.3333333  0.6666667 -0.33333333
## [3,]  0.5833333 -0.4166667  0.08333333