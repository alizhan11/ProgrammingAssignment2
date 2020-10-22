## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Create a special object which can cache its inverse.
## Object contains:
## 1. internal member variables which store the matrix and its possibly cached inverse
## 2. the public functions by calling them we can implement the cache of the inverse
##      of the given function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(y) {
                m <<- y
        }
        getinv <- function(y) m
        list(set = set, get = get,
             setinv = setinv, 
             getinv = getinv)
        
}


## Write a short comment describing this function
## Function of returning the inverse of the given matrix
## If the inverse has been calculated, then function should retrieve it from cache
## x -> special "matrix" created by MakeCacheMatrix function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        ## If the inverse of the given matrix has been cached
        # (and the matrix hasn't changed)
        if(!is.null(inv)) {
                # just return it
                return(inv)
        }
        # Else compute the inverse and cache it into the special object, 
        # and then return the inverse
        inv <- solve(x$get())
        x$setinv(inv)
        inv
}
