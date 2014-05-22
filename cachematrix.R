## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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

## Write a short comment describing this function

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