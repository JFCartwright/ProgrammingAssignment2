## Calculating the inverse of large matrices can be time consuming and so in
## iterative processes it can be useful to avoid the constant re-computation
## of the matrix inverse and instead store the matrix inverse to save this 
## time on computation. The first function here creates a matrix with extra
## assigned values relating to its inverse (if this has not yet been done
## then values will be NULL). The second function accesses the first function's 
## environment to make use of these extra assigned values so that if the inverse
## has already been calculated then it can just be printed. If it has not yet 
## been calculated then it will see this and calculate the inverse and assign
## this to the objects in the first function environment for future use.


## makeCacheMatrix function sets the value of the matrix, gets the
## value of the matrix, set the value of the inverse and gets the
## value of the inverse. This is passed to function cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve function calculates the inverse of a matrix. If this
## has already been done then the function will return a stored version
## of this inverse so that the computation of this calculation can be skipped.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
