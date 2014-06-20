## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#This function creates an object of special Matrix for Assigment #2
# that can cache its inverse matrix


makeCacheMatrix <- function(x = matrix()) {
        # First let's make sure m is null
        m <- NULL
        #creates set that load matrix into x and sets m to null 
        #so the cache logic works in case of residual value when called
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        # creates get that prints the value of x when called
        get <- function() x
        # creates setinverse that sets the value of m when called
        setinverse <- function(inverse) m <<- inverse
        # creates getinverse that prints the value of m when called
        getinverse <- function() m
        # Creates a List of all the methods for this function
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
        
}


## Write a short comment describing this function

# This function checks if an inversed matrix cached already for
# the matrix in the calling function, in this project it is 
# "makeCacheMatrix". If inversed matrix is in cache, then returnes it to the 
# calling function, with a message.  Otherwise, it creates the 
# Inverse of the suplied matrix and returns the inverse to the
# calling function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # First we check the calling function if an inverse exists
        m <- x$getinverse()
        # If so, we print a message capturing the existance of cache
        # and return the inverse from cache to the calling function
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        # We get the matrix from the calling function into data
        data <- x$get()
        # here we generate the inverse matrix into m
        m <- solve(x$get()) # solve(data, m)
        # and we send m, as inversed matix back into the calling function
        x$setinverse(m)
        # we print the inversed matrix
        m
}

        
        
        
        