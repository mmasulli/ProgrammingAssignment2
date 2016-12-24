## makeCacheMatrix is a function used to create a special "matrix" object
## whose inverse can be kept in cache.

## "I" stands for inverse matrix
## "SetI" is used to stablish the inverse of "I"
## "getI" is a function to obtain the value of "I"
## "list" is used to present all the values

makeCacheMatrix <- function(X = matrix()) {
        I <- NULL
        set <-function(Y){
                X <<- Y
                I <<- NULL
        }
        get <- function() X
        setI <- function(inv) I <<- inv
        getI <- function() I
        list(set = set, get = get,
             setI = setI,
             getI = getI)
}


## CacheSolve is used to compute the inverse of the matrix assuming it is an
## invertible square matrix
## "R" is the inverse of the matrix that you get as a Result.
cacheSolve <- function(x, ...) {
        R <- x$getI()
        if(!is.null(R)) {
                message("getting cached data")
                return(R)
        }
        data <- x$get()
        R <- solve(data)
        x$setI(R)
        R
}
