# Programmong Assignment 2

# This function creates a special "matrix" object 
# that can cache its inverse.
# Mostly based on code from example "Caching the Mean of a Vector"
## Arguments: 
##  x - something numeric, empty by default
##      (matrix is a vector with extra dimension)
makeCacheMatrix <- function(x = numeric()) {
    m <- NULL
    # actions to set matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    # actions to get matrix
    get <- function() x
    # actions to set inverse matrix
    setsolve <- function(solve) m <<- solve
    # actions to get inverse matrix
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)    
}

# This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix()
# Timer was added just because I'm curious
## Arguments: 
##  x - cashed matrix object, 
##      created by makeCacheMatrix()
##  ... - any detailed settings
cacheSolve <- function(x, ...) {
    # start timer
    start <- strftime(Sys.time(), format = "%Y-%m-%d %H:%M:%S")
    
    m <- x$getsolve()
    
    if(!is.null(m)) {
        # say user that inverse matrix was already calculated
        print(message("getting cached data"))
        # stop timer
        print(paste(as.numeric(difftime(strftime(Sys.time(), 
                                                 format = "%Y-%m-%d %H:%M:%S"), 
                                        t)),
                    "seconds"))
        # return result
        return(m)
    }
    # calculate for the first time
    data <- x$get()
    m <- solve(data, ...)    
    x$setsolve(m)
    # stop timer
    print(paste(as.numeric(difftime(strftime(Sys.time(), 
                                             format = "%Y-%m-%d %H:%M:%S"), 
                                    t)),
                "seconds"))
    # return a matrix that is the inverse of 'x'
    m    
}
