## Caching the Inverse of a Matrix

## This function create a special "matrix" object from matrix 'x'

makeCacheMatrix <- function(x = matrix()) {
        ## Check for correct type of matrix for this task
        if(dim(x)[1] != dim(x)[2]) {
                message('Wrong matrix!')
                return()
        }
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

## This function calculate the inverse of matrix 'x'

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
