## The functions below cache the inverse of a matrix

makecachematrix <- function(x = matrix()){              ## Cache Matrix function
  inv <- NULL                                           ## Initialize the inverse property
  set <- function(y){                                   ## Method to set the matrix
    x <<-y
    inv <<-NULL
  }
  get <- function() {x}                                 ## Method the get the matrix
  setinverse <- function(inverse) {inv <<- inverse}     ## Method to set the inverse of the matrix
  getinverse <- function(){inv}                         ## Method to get the inverse of the matriix
  list(set = set ,                                      ## Return a list of the methods
       get = get, 
       setinverse  =  setinverse , 
       getinverse  =  getinverse)
}

## Compute the inverse of a special matrix returned by "makeCacheMatrix"
## above. If the inverse is already calculated (and the matrix has not changed)
## Then the "cachesolve" should retrieve the inverse from the cache


cachesolve <- function(x, ...){
  inv <- x$getinverse()                                 ## Return a matrix that is the inverse of 'x'
  if(!is.null(inv)){                                    ## Just return the inverse if its already set
    message('getting cached data')
    return(inv)
  }
  mat <- x$get()                                        ## Get the matrix from our object
  inv <- solve(mat, ...)                                ## Calculate the inverse using matrix multiplication
  x$setinverse(inv)                                     ## Set the inverse to the object
  inv                                                   ## Return the matrix
}
