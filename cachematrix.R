## The first function is the Cache Matrix functio
## function calculating the inverse of the matrix

## we are creating a funcion that which gives the list of elements as output

makecachematrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x <<-y
    inv <<-NULL
  }
  get <- function() {x}
  setinverse <- function(inverse) {inv <<- inverse}
  getinverse <- function(){inv}
  list(set = set , get = get, setinverse =  setinverse , getinverse = getinverse)
}

## the below function helps in holding the data with in the environment without calculating all the computations
cachesolve <- function(x, ...){
  inv <- x$getinverse()
  if(!is.null(inv)){
    message('getting cached data')
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinverse(inv)
  inv
}
