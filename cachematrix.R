## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# This function creates a list of matrix functions 
# which can be executed on passed matrix variable
makeCacheMatrix <- function(x = matrix()) {

        #initilize inverseX variable with NULL to make 
        #sure we do not get any garbage data
        inverseX <- NULL
        
        # create a function to set passed in matrix to x
        set <- function(y) {
                x <<- y
                inverseX <<- NULL
        }
        
        # create a function to return x
        get <- function() x
        
        # create a function to assign inverse matrix to inverseX variable
        setInverse <- function(inverse) inverseX <<- inverse
        
        # create a function return inverseX
        getInverse <- function() inverseX
        
        # return the list of functions created above for
        # the matrix get / set and inverse matrix get/set methods
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function
# This function solves the inverse matrix of supplied matrix
cacheSolve <- function(x, ...) {
        
        # try to get the inverse matrix of x if its already created
        inverseX <- x$getInverse()
        
        # If inverse matrix of x is found in cache, return the cached data
        if(!is.null(inverseX)) {
                message("getting cached data")
                return(inverseX)
        }
        
        # if function did not return previously with cached return 
        # matrix, that means there is no cached data. 
        # We will need to create inverse of x now.
        
        # get matrix and assign to data varoable
        data <- x$get()
        
        # create inverse if x using solve() function
        inverseX <- solve(data, ...)
        
        # cache the created inverse matrix into memory
        x$setInverse(inverseX)
        
        #return inverse matrix
        inverseX
}
