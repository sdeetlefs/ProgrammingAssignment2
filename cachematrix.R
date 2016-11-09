## These two functions store the value of the inverse 
## of the matrix passed as in argument and calculate
## the inverse of a new matrix if necessary

## This function stores the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
        
}


## This function returns the inverse of the matrix
## if the contents of the matrix is null, otherwise
## the cached matrix is returned

cacheSolve <- function(x, ...) {
        
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
