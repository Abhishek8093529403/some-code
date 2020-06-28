## A pair of functions that cache the inverse of a matrix

## Write a short comment describing this function
## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function( p = matrix() ) {

	## Initialize the inverse property
    i <- NULL

    ## Method to set the matrix
    set <- function( matrix ) {
            p <<- matrix
            i <<- NULL
    }

    ## Method the get the matrix
    get <- function() {
    	## Return the matrix
    	p
    }

    ## Method to set the inverse of the matrix
    setInverse <- function(inverse) {
        i <<- inverse
    }

    ## Method to get the inverse of the matrix
    getInverse <- function() {
        ## Return the inverse property
        i
    }

    ## Return a list of the methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

    ## Return a matrix that is the inverse of 'x'
    p <- x$getInverse()

    ## Just return the inverse if its already set
    if( !is.null(p) ) {
            message("getting cached data")
            return(p)
    }

    ## Get the matrix from our object
    data <- x$get()

    ## Calculate the inverse using matrix multiplication
    p <- solve(data) %*% data

    ## Set the inverse to the object
    x$setInverse(p)

    ## Return the matrix
    p
}
