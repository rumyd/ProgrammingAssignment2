## The 2 functions below are used to a)create special matrix object that can 
## cache its inverse and b)compute the inverse of special object created by 
## first function

## Function makeCacheMatrix returns a special matrix object (list of functions)
## that can cache its inverse


makeCacheMatrix <- function(x = numeric()) {
    i <- NULL 
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    
    get <- function() {  x }
    setinverse <- function(inverse) { i <<- inverse }
    getinverse <- function() { i }
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
    
}


## cacheSolve returns inverse of matrix from cache, if available, else computes
## and returns a matrix that is an inverse of 'x'

cacheSolve <- function(x, ...) {
    
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached inverse")
        return(i)
    }    
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i    
}
