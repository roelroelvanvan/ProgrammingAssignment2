## I basically used the same format as the example for the assignment. 
## Instead of using mean(), i used solve()

## This function takes a matrix as input, and outputs a list of 4 functions. 
## Set sets a new matrix and deletes any cached inverse. Get returns the matrix. 
## Setinv is used by the second function to store the calculated inverse. 
## Getinv is used to get the cached inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL # cached inverse will be stored here
    set <- function(y) {
        x <<- y # sets a new matrix 
        i <<- NULL # deletes any existing chached inverse 
    }
    get <- function() x 
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function takes the output from the previous function as input, and outputs 
## either the cached inverse, or calculates the inverse of the matrix.

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(!is.null(i)) { #checks if there is already a cached inverse, and returns it
        message("getting cached data")
       return(i)
    } else { # calculates the inverse if there is no chached version
    data <- x$get()
    i <- solve(data)
    x$setinv(i)
    i    
    }
}
