# hi there. thanks for the review!
# The method I have used is to a very large extent based on the example given.
# except that it calculates a matrix's inverse, instead of a vector's mean.
# The function makeCacheMatrix stores a list of four functions: set, get, setinv
# and getinv.
makeCacheMatrix <- function(x = numeric()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        # The function "set" changes the matrix stored in the main function,
        # only if the matrix is to be changed. The <<- sign means that the values
        # are changed in the main function. i <<- NULL makes sure that a new inverse
        # calculated in cacheSolve.R
        get <- function() x
        # The function "get" returns the matrix x stored in the main function. 
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        # The above two functions are supposed to store (setinv) and load (getinv)
        # the inverse of matrices which have already been solved.
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        # The list function above simply stores the four functions defined here in a list.
}


# The input of this following function is the object that comes out of the 
# previous one. First it verifies whether the value x$getinv() is null. If it has 
# a value, which is supposed to be an already calculated inverse, this value is
# returned, and the rest is skipped. Otherwise (if it is null), the inverse of
# the input is calculated.
cacheSolve <- function(x) {
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, diag(nrow(data)))
        # diag(nrow(data)) creates an identity matrix as big as the number of rows
        # / columns in "data". And therefore the SOLVE function gives the inverse
        x$setinv(i)
        i
}