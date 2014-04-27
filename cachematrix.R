## This code is the week 3 homework for the Intro to R Programming Course on Coursera via Johns Hopkins
## It contains a bit of code that implements a caching function to store the inverse of a matrix so that
## a cached copy is created on first use and is then used in each subsequent call to inverse the matrix

## This is the function that caches the inverse matrix
## makeCacheMatrix is strictly a property bag.  This differs from the sample provided slightly
## If the instructions did not explicitly say this, I would likely put the inverse in the makeCacheMatrix function
## The constructor would also set inv explicitly on the first set

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL

    set <- function(y) {
        ## Validate whether or not this is a new matrix - no point in clearing the cache if it is not
        if (is.na(x) || any(x != y)) {
            ## Reset the inverse cache if it is a new matrix
            x <<- y
            inv <<- NULL
        }
    }
    get <- function() x
    setinv <- function(y) {
        inv <<- y
    }
    getinv <- function() inv 

    # Return the object a list of named functions that act as methods on the object
    list(
        set = set,
        get = get,
        setinv = setinv,
        getinv = getinv
    )
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.
## As per this definition, the actual calculation of the inverse happens here.  

cacheSolve <- function(x, ...) {
    ## It would be nice if I could use the message stream in my unit tests
    ## I'll have to figure out the best way to do something like this in R
    inv <- x$getinv()
    if (!is.null(inv)) {
            message("getting cached data")
            return(inv)
    } 
    message("Not in cache - calculating inverse")
    x$setinv(solve(x$get()))
    ## Return a matrix that is the inverse of 'x'
    return(x$getinv())
}
