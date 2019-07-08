

## This function takes a matrix as input, and creates a special matrix object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    invMatrix <- NULL                   # create a placeholder for the cached inverse
    setMatrix <- function(y){
        x <<- y
        invMatrix <<- NULL
    }
    
    getMatrix <- function() x  
    setInv    <- function(inv) {
        invMatrix <<- inv
    }
    getInv <- function() invMatrix
    list(setMatrix = setMatrix, getMatrix = getMatrix, setInv = setInv, getInv = getInv)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve retrieves 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
    m <- x$getInv()              ## Get the inverted matrix from the cache (could be empty)
    
    ## if the cache has the inverse, load the existing value of the inverted matrix
    if(!is.null(m)){
        message("Getting cached inverted matrix")
        return(m)
    }
    
    ## if the cache is NULL, then compute the inverse of the matrix
    data <- x$getMatrix()
    m <- solve(data, ...)
    x$setInv(m)
    m                             ## Return a matrix that is the inverse of 'x'
    
}


## TEST THE FUNCTIONS -----------------

TestMatrix <- matrix(c(9,3,1,-3),2,2)
TestMatrix

CacheMatrix <- makeCacheMatrix(TestMatrix)
CacheMatrix$getMatrix()
CacheMatrix$getInv()

cacheSolve(CacheMatrix)
cacheSolve(CacheMatrix)

## TEST A BIGGER MATRIX

TestMatrix <- matrix(c(5,2,6,2,6,2,6,3,6,2,2,6,8,8,8,5),4,4)
TestMatrix

CacheMatrix <- makeCacheMatrix(TestMatrix)
CacheMatrix$getMatrix()
CacheMatrix$getInv()

cacheSolve(CacheMatrix)
cacheSolve(CacheMatrix)
