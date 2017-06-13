## makeCacheMatrix function creates a special "matrix". It has 4 functions:
## set() - sets the value of the input matrix 
## get() - gets the value of the input matrix
## setinverse () - sets the value of the inverse of the matrix
## getinverse () - gets the value of the inverse of the matrix

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


## cacheSolve function returns a matrix that is the inverse of 'x'
## This function first checks to see if the Inverse has already been calculated
## If so, it gets the Inverse from the cache using getinverse function and skips the computation.
## Otherwise, it calculates the Inverse of the matrix (using solve function) and sets the value of the Inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Input Matrix: x
        ## Output (Inverse Matrix) : i
        i <- x$getinverse()
        
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
