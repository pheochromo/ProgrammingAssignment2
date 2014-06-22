## Together these functions create a special matrix
## whose inverse can be cached for later retreival

## This function returns a list containing functions to
## set/get matrix, and set/get its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        ## uses scoping assignment to set matrix in parent environment
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        ## retreive matrix
        get <- function() x
        
        ## sets passed argument to inv for retreival
        setinv <- function(inverse) inv <<- inverse
        
        ## retreive inverse
        getinv <- function() inv
        
        ## return list containing above functions
        list (set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function checks if inverse is cached, if so returns inverse.
## Otherwise, calculate inverse with solve() and cache it

cacheSolve <- function(x, ...) {
        ## get inverse from special matrix
        inv <- x$getinv()
        
        ## if inverse !null, return inv
        if (!is.null(inv)) {
                message("Getting cached data")
                return(inv)
        }
        
        ## if no inverse, get matrix and solve for inverse
        matrix <- x$get()
        inv <- solve(matrix, ...)
        
        ## cache inverse and return it
        x$setinv(inv)
        inv
}