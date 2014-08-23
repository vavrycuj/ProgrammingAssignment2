## These two functions chache the inverse of a matrix 


## This function sets the parameters which are needed for function cacheSolve

makeCacheMatrix <- function(x = numeric()){
    i <- NULL # i stores the inverse and is reset every time the function makeCacheMatrix is called
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x # returns the original value of the input
    setinverse <- function(inverse) i <<- inverse # assigns the inverse to the i parameter
    getinverse <- function() i # returns the inverse
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  # list of output parameters  
}



## This function computes the inverse of a matrix, in case that the particular matrix have not been an input, yet.
## In the other case, it returns the cached data.

cacheSolve <- function(x,...) {
    i <- x$getinverse() # reads the inverse of the object x
    if(!is.null(i)) { # if the inverse already has been computed...
        message("getting cached data")
        return(i) # ...it returns the cached data as an output/result
    }
    data <- x$get() # otherwise, it gets the original input (square matrix)
    i <- solve(data,...) # solves the inversion
    x$setinverse(i) # set the inverse parameter for x object
    i # returns the computed inverse as an output/result
}
