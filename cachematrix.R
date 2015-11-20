## R Functions for caching the inverse of a matrix
## The inverse of a matrix is calculated once and saved.
## Subsequent requests for the inverse are retrieved from saved value.

## makeCacheMatrix - This function creates an cacheMatrix
## object with getter/setter methods for the matrix and it's inverse 

makeCacheMatrix <- function(x = matrix()) {
        theInverse <- NULL
        set <- function(y) {
                x <<- y
                theInverse <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) theInverse <<- inverse
        getInverse <- function() theInverse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve - This function returns the saved matrix inverse value, if one
## exists. Otherwise, it computes the inverse and saves it for future reference. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        theInverse <- x$getInverse()
        if(!is.null(theInverse)) {
                message("getting cached data")
                return(theInverse)
        }
        data <- x$get()
        theInverse <- solve(data, ...)
        x$setInverse(theInverse)
        theInverse        
}
