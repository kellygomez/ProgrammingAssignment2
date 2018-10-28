## i = inverse matrix, x= the matrix.

## This function set and gets the matrix; and set and gets the inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## This function calculate the inverse of the matrix if it has not been calculate
# if it is calculated returns the inverse

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(is.matrix(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}
