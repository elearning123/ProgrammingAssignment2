## This function creates a matrix with 4 behaviours: 
## set and get matrix and its inverse 

makeCacheMatrix <- function(x = matrix()) {
  
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
       
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}
    

## This function calculates inverse of a square matrix and caches the value
## so it gets retrieved rather than recalculated

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, diag(nrow(data)))
    x$setinverse(m)
    m
    }
