## Create a matrix object that can store and return the cached value of its inverse


## Matrix object with getter and setter methods for the matrix itself and its inverse

makeCacheMatrix <- function(x = matrix()) {
        
        inverse <- NULL
        
        #Assign a matrix to the object
        set <- function(y) {
                #Since the value of the matrix has changed, set the value of the invers to null
                x <<- y
                inverse <<- NULL
        }
        
        #Return the matrix object
        get <- function() x
        
        #Set the inverse of the matrix
        setInverse <- function(new_inverse) inverse <<- new_inverse
        
        #Return the inverse of the matrix
        getInverse <- function() inverse
        
        #Return a list of the four function attributes in the object
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Retrieve the cached inverse of a matrix
## Incase it is not cached, calculate the inverse and cache it within the matrix object

cacheSolve <- function(x) {
        
        #Obtain the value of the inverse matrix stored in the object
        inverse <- x$getInverse()
        
        #If the value is not NULL, use the cached inverse matrix
        if(!is.null(inverse)) {
                message("Retrieving cached data")
                return(inverse)
        }
        
        #Since cached inverse is not available, obtain the original matrix
        data <- x$get()
        
        #Calculate the inverse of the matrix
        inverse <- solve(data)
        
        #Store the new inverse matrix in the object
        x$setInverse(inverse)
        
        #Return the new inverse matrix
        inverse
}

