## The following functions calculate the inverse of a matrix and saves it
## to the cache such that the next time the user attempts to calculate the
## matrix inverse, the previously saved value is returned instead of
## repeating the calculation.

makeCacheMatrix <- function(x = matrix()) {
        ## create a matrix object x and some associated sub-functions/methods
        
        ## define the cache m
        m <- NULL
        set <- function(y) {
                x <<- y 
                m <<- NULL 
        }
        get <- function() x ## return the matrix x
        setinverse <- function(inverse) m <<- inverse ## set the cache m equal
        ## to the inverse of the matrix x
        getinverse <- function() m ## return the cached inverse of x
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The following function calculates the inverse of the special "matrix" created
## with the above function. 

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
