## A matrix in special format is created and its inverse is taken. 
## The inverse is not taken if it is already taken in the previous
## operations. 

## makeCacheMatrix(x=matrix(1:4,2,2)) creates a list:
## x$get outputs the matrix itself
## x$set assigns a new matrix into it
## x$setInverse computes and stores the inverse
## x$getInverse outputs the inverse

makeCacheMatrix <- function(x = matrix()) {
        ## create a matrix as a list, 
        ## x$get is the matrix itself
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve(x) solves the matrix which has to be inputted as a list
## defined by makeCacheMatrix(x=matrix())
## inputted matrix has to be square invertible
## if the inverse is already in the cache, it is pulled form the cache 
## directly, no computation of inverse is done. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of x
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}