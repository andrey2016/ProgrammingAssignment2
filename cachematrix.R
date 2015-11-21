## These functions create a matrix object, find its inverse and save it in cash,
## so there is no need to solve it again, if the inverse is alredy in the cash

## This function creates a matrix object and cashes its inverse

makeCacheMatrix <- function(x = matrix()) {
 m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function finds the inverse matrix after searching it in cash 

cacheSolve <- function(x, ...) {
      m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}