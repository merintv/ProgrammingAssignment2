## Creating object to cache the inverse
makeCacheMatrix <- function(a = matrix()) {
inverse_matrix <- NULL
    set <- function(b)
    {
    a <<- b
    inverse_matrix <<- NULL
    }
    get <- function() a
    setinverse <- function(inverse) inverse_matrix <<- inverse
    getinverse <- function() inverse_matrix
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Getting the inverse from the cache if inverse is already got calculated

cacheSolve <- function(a, ...) {
         inverse_matrix <- a$getinverse()
    if(!is.null(inverse_matrix)) 
    {
      return(inverse_matrix)
    }
    data <- a$get()
    inverse_matrix <- solve(data)
    a$setinverse(inverse_matrix)
    inverse_matrix
}
