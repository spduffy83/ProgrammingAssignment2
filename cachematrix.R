## makeCacheMatrix and cacheSolve defines a list that includes functions to interogate
## a matrix and returns the inverse of an inputted matrix


##The first function, `makeCacheMatrix` creates a special "vector", which is
##really a list containing a function to
##1.  set the value of the vector
##2.  get the value of the vector
##3.  set the value of the inverse of a matrix
##4.  get the value of the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Calculates the inverse of a matrix created with the above function
## However, it first checks to see if the inverse has already been calculated.
## If so, it `get`s the inverse from the cache and skips the computation. Otherwise, 
## it calculates the inverse of the data and sets the value of the inverse in the cache
## via the `setInverse` function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached inversed data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
