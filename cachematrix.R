#  Caching the Inverse of a Matrix

#  The matrix supplied here should be invertible

# The first function, makeCacheMatrix creates a special "matrix" object that can cache its inverse

# makeCacheMatrix returns a list, using which you can 
# get the contents of associated matrix
# set associated matrix
# get the inverse of associated matrix
# set the inverse of associated matrix

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) s <<- solve
    getinverse <- function() s
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# cacheSolve calculates the inverse of the special "matrix" returned by makeCacheMatrix above  

# If the inverse has already been calculated, then this function gets the inverse from the cache

cacheSolve <- function(x, ...) {
    s <- x$getinverse()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setinverse(s)
    s
}
