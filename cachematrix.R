## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) i <<- inverse
        getInverse <- function() i
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## Write a short comment describing this function
## This calculates the inverse of the special "matirx" creates above. 
## By first checking if the inverse has already been calculated if so then it skips the 
## calcuation and gets inverse from cache. Otherwise it calculates its inverse and sets
## the inverse in the cache via setInverse. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}
