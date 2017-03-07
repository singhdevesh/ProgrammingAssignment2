

##2 functions created- a special matrix that can cache its inverse
## and compute the inverse of the special matrix object

## The first function below creates a special matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        I <- NULL
        set <- function(y){
                x <<- y
                I <<- NULL
          
        }
        get <- function() x
        setInverse <- function(inverse) I <<- inverse
        getInverse <- function() I
        list(set = set, get = get,
             setInverse = setInverse, getInverse = getInverse)
}


## the following function- cacheSolve computes the 
## inverse of the special matrix returned by makeCacheMatrix above
## if the inverse has already been calculated (and the matrix has not changed)
## the cacheSolve should retrieve the inverse from the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
        I <- x$getInverse()
        if (!is.null(I)){
                message("getting cached data")
                return(I)
        }
        
        mtx <- x$get()
        I <- solve(mtx, ...)
        x$setInverse(I)
        I
}
