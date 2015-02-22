## The first function, makeCacheMatrix creates a special "vector", 
## which is really a list containing a function to:
##
## set the value of the matrix
## get the value of the matrix
## get the value of the solved (inversed) matrix
## set the value of the solved (inversed) matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL 
        }
        get <- function() x
        setSolve <- function(solve) m  <<- matrix
        getSolve <- function() m
        list(    set = set
               , get = get
               , setSolve = setSolve
               , getSolve = getSolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getSolve()
        if(!is.null(m)) {
                ## there alredy is a computed inversed value for the matrix.
                ## We better use this value, it's 'cheaper'.
                message("getting cached data")
                return(m)
        }
        ## No inversed value found. We must calculate it.
        data <- x$get()
        m <- Solve(data, ...)
        x$setSolve(m)
        m
}
