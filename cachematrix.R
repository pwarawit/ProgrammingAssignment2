## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    
    # Initialize m variable as temporary object
    m <- NULL
    
    # define function set, get, setsolve and getsolve
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    
    # return list of functions
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
    
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    m <- x$getsolve()
    if(!is.null(m)) {
        # If there is cached data, then return cached data
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    # if not, then just solve the matrix
    m <- solve(data, ...)
    # also keep cached for future uses too
    x$setsolve(m)
    m
}
