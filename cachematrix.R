## Making vector for matrix inverse cache
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


## Solving matrix cache
cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached inversed matrix")
        return(i)
    }
    matrixData <- x$get()
    i <- solve(matrixData, ...)
    x$setinverse(i)
    i
    
}