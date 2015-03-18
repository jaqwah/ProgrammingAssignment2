## Cache the inverse of a matrix rather than compute it repeatedly using the 
## following two functions. 

## Create a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
     c <- NULL ## Initialize variable by setting to null
               ## also overrides any previous value when assigning new matrix 
     set <- function (y) { ## define function to set the value of the matrix
          x <<- y        ## '<<-' operator assigns x & c to parent enviroment 
          c <<- NULL     
         
     }
     get <- function () x ## define a function to get the value of x
     setinv <- function(inv) c<<-solve(x)
     ## define a function to assign a new value to c in parent enviro. new value
     ## is inverse of matrix x - will be called in the cacheSolve function below
     getinv <- function() c
     ## get the value of c stored in the parent enviro
     list(set=set, get=get, 
          setinv=setinv, getinv=getinv)
     ## lists the functions
}

## Computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not 
## changed), then the cachesolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) { ## Return a matrix that is the inverse of 'x'
     c2 <- x$getinv()     ## lookup cached value of inverse matrix 
     if(!is.null(c2)) {   ## determine if the matrix has been evaluated before  
          message("getting cached data") ## if it has, prints message
          return(c2)                     ## and the value of c2
     }
     data <- x$get()  ## If x has not been evaluated before, pull from
                      ## makeCacheMatrix function into a variable named 'data'
     c2<- solve(data, ...)  ## calcuate the inverse of the 'data' variable
     x$setinv(c2)  ## Assign the value to the setinv function
     c2
}
