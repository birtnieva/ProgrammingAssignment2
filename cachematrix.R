## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function takes a matrix and stores it in its environment 
## variable x. The function creates a vector of four functions:
## (1) to set value of matrix, (2) to get value of matrix,
## (3) set the value of cache and (4) retrieve the cache contents.

makeCacheMatrix <- function(x = matrix()) {
        ix <- NULL
        set <- function(y) {
                x <<- y
                ix <<- NULL
        }
        get <- function() x
        setcache <- function(solve) ix <<- solve
        getcache <- function() ix
        list(set = set, get = get,
             setcache = setcache,
             getcache = getcache)
}


## Write a short comment describing this function
## This function calculates the inverse of the matrix from the
## previous function. It checks if the inverse is in cache, gets
## the content of the cache and skips the inverse operation. If it is not 
## in cache, it checks if the matrix is invertible and proceeds 
## to solve for the inverse. Else it returns a message that the 
## matrix in no invertible.   

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Return a matrix that is the inverse of 'x'
        ix <- x$getcache()
        if(!is.null(ix)) {
                message("getting cached data")
                return(ix)
        }
        mtrx <- x$get()
        if(det(mtrx) != 0) {
                ix <- solve(mtrx, ...)
                x$setcache(ix)
                ix
        } 
        else {
                message("matrix is not invertible")
        }
}
