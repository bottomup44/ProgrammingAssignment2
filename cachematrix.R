## The following two functions will do the two things.  
## First, the first function makes a matrix object which can cache its inverse.
## Second, the second function compute the inverse of the matrix 
## creaed by the first function. 
## If the inverse has already been computed, and the matrix has not changed, 
## the second function will not compute it again
## and instead retrieve the inverse from the cache. 

## The first function, makeCacheMatrix, will create a matrix object,
## and it also caches its inverse computed by the second function (setimat). 

makeCacheMatrix <- function(x = matrix()) {
    imat <- NULL
    set <- function(y,r,c) {
        x <<- matrix(y, nrow=r, ncol=c)
        imat <<- NULL
    }
    get <- function() x
    setimat <- function(invM) imat <<- invM
    getimat <- function() imat
    list(set = set, get = get,
         setimat = setimat,
         getimat = getimat)
}

## The second function computes the inverse of the matrix created by makeCacheMatrix.
## However, if it has already been calculated and cached, 
## it will retrieve the inverse from the cache,
## and will show the message "getting cached matrix")

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    imat <- x$getimat()
    if(!is.null(imat)) {
        message("getting cached matrix")
        return(imat)
    }
    d <- x$get()
    imat <- solve(d)
    x$setimat(imat)
    imat
}