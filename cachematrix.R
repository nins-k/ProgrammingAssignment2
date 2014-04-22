## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(new_inverse) inverse <<- new_inverse
        getInverse <- function() inverse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

cacheSolve <- function(x) {
        new_inverse <- x$getInverse()
        if(!is.null(new_inverse)) {
                message("Using cached data")
                return(new_inverse)
        }
        data <- x$get()
        new_inverse <- solve(data)
        x$setInverse(new_inverse)
        new_inverse
}

