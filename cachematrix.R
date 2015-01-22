## makeCacheMatix and cacheSolve work together to set, get, and cache 
## a matrix and its inverse.


## makeCacheMatrix is a function that creates and returns a list of functions for setting,
## getting, and caching a matrix and its inverse.

makeCacheMatrix <- function(mtx = matrix()) {
    inv_mtx <- NULL
    set <- function(new_mtx) {
        mtx <<- new_mtx
        inv_mtx <<- NULL
    }
    get <- function() mtx
    setinv_mtx <- function(inverse_mtx) inv_mtx <<- inverse_mtx
    getinv_mtx <- function() inv_mtx
    
    ## Return a list of the functions defined above
    list(set = set, get = get,
         setinv_mtx = setinv_mtx,
         getinv_mtx = getinv_mtx)
}


## cacheSolve does the work of calculating the inverse of 
## the matrix.  It calls the functions of makeCacheMatrix to get and cache the matrix
## and its inverse.
## cacheSolve returns the inverse matrix.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv_mtx <- x$getinv_mtx()
    if(!is.null(inv_mtx)) {
        message("getting cached data")
        return(inv_mtx)
    }
    data <- x$get()
    inv_mtx <- solve(data)
    x$setinv_mtx(inv_mtx)
    inv_mtx
}
