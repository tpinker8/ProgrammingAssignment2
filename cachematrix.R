## The following functions makeCacheMatrix and cacheSolve cache the inverse of a matrix 
## because computing the inverse of a large matrix can be very time consuming especially
## if it is required to compute the same inverse repeatedly.

## the Function "makeCacheMatrix" creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

## initialization of two objects, x and i.  
## they are defined in the parent environment and available outside of the function
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        
## The input value is assigned to i in the set function
        setinverse <- function(inverse) i <<- inverse
## This is the get function        
        getinverse <- function() i
        
## the for functions are assigned to a list which is the output 
## of the makeCacheMatrix function
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The function "cacheSolve" computes the inverse of the special "matrix" 
## returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve function retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {

        i <- x$getinverse()
        
## Check to see if there is already an inverse in cache (not null), 
## if it is not null then it prints message "getting cached data" 
## and retrieves the inverse
        
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }

## If the cache is null then it computes the inverse and sets it to cache        
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)

## outputs the inverse of the input matrix        
        i
        
}
