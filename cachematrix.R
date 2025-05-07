## Put comments here that give an overall description of what your
## functions do

## This function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
     
                I <- NULL
                set <- function(y) {
                        x <<- y
                        I <<- NULL
                }
                get <- function() x
                setInverse <- function(inverse) I <<- inverse
                getInverse <- function() I
                list(set = set, get = get,
                     setInverse = setInverse,
                     getInverse = getInverse)
        }



## This function computes the inverse of the special matrix returned by makeCacheMatrix.
## If the inverse has already been calculated and cached, it retrieves it from the cache.

cacheSolve <- function(x, ...) {
        
                S <- x$getInverse()
                if(!is.null(S)) {
                        message("getting cached data")
                        return(S)
                }
                mat <- x$get()
                S <- solve(mat, ...)
                x$setInverse(S)
                S
        }
        
        
        ## Return a matrix that is the inverse of 'x'



