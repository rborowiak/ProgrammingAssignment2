# Programming Assignment 2: Lexical scoping - cachinig the inverse of a matrix 

## The first function, `makeCacheMatrix` creates a special "matrix", which is
##really a list containing a function to set/get matrix and their inverse

# define matrix object
makeCacheMatrix <- function(X = matrix()) {
        X_inverse <- NULL
        set <- function(y) {
        # assigns value on the right side of the assignment-operator 
        # to an object in the parent environment
        X <<- y  
        X_inverse <<- NULL
        }
        get <- function() X
        setinverse <- function(inverse) X_inverse <<- inverse
        getinverse <- function() X_inverse
        
        # set/get functions are returned as a list to the parent environment
        list(set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse)

}

## The following function calculates the inverse of the special "matrix"
## created with the above function and caches it

cacheSolve <- function(X, ...) {
        X_inverse <- X$getinverse()
        
        # checks if the inverse of the matrix is already in cache.
        # If so the value in cache  is retrieved and computation stops
        if(!is.null(X_inverse)) {   
          message("getting cached data") 
          return(X_inverse)
        }
        
        # Otherwise Matrix is loaded
        data <- X$get()
        # inverse matrix of X is calculated by function solve and the value of the 
        # inverse is set in the cache 
        X_inverse <- solve(data, ...) 
        X$setinverse(X_inverse)
        
        ## Return a matrix that is the inverse of 'X'
        X_inverse
        
}
