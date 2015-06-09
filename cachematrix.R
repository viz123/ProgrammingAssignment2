## Put comments here that give an overall description of what your
## functions do

# There are two functions here. The first function creates a wrapper around a given matrix
# and the second function uses the wrapper to return the cached inverse of the matrix 

## Write a short comment describing this function

# This function creates and returns a list of accessors for a given matrix as well as its 
# inverse. The inverse is stored in a global variable by the function so that its value is 
# retained even after the function returns

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(Inv) i <<- Inv
        getInverse <- function() i
        list(set=set, get=get, setInverse = setInverse, getInverse = getInverse)
}

## Write a short comment describing this function

# This function takes the list of accessors returned by makeCacheMatrix function and returns
# the inverse of the matrix wrapped by the makeCacheMatrix function.

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
        I <- x$getInverse()
        if (!is.null(I)){
                return (I)
        }
        data <- x$get()
        I <- solve(data)
        x$setInverse(I)
        I
}
