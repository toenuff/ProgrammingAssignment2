
## Using hilbert function found in solve documentation to create a matrix that can be inverted
hilbert <- function(n) {i <- 1:n; 1/outer(i-1, i, "+")}
x <- hilbert(4)
xinv <- matrix(nrow=4,ncol=4)
xinv[1,] <- c(16,-120,240,-140)
xinv[2,] <- c(-120,1200,-2700,1680)
xinv[3,] <- c(240,-2700,6480,-4200)
xinv[4,] <- c(-140,1680,-4200,2800)

## considered doing the multiplication for this, but it's not really needed
## Keeping imatrix in unit test because I'm using it as a spare matrix for makeCacheMatrix testing
imatrix <- matrix(nrow=4,ncol=4)
imatrix[1,] <- c(1,0,0,0)
imatrix[2,] <- c(0,1,0,0)
imatrix[3,] <- c(0,0,1,0)
imatrix[4,] <- c(0,0,0,1)

test.makeCacheMatrix <- function() {
    ## Need to look into whether or not Runit tests can simply test for success and failure.
    ## This would be good for the following line and all set functions.  However, I'm way away
    ## from the Internet now.  Probably won't fix before submission.
    cache = makeCacheMatrix()
    ## Ensure that we can set and retrieve the matrix from the cache
    cache$set(x)
	checkEquals(x, cache$get())

    ## Initial create should not set the inv value
    checkEquals(NULL, cache$getinv())

    ## Ensure that we can set and retrive the cached inverse matrix
    cache$setinv(xinv)
    checkEquals(xinv, cache$getinv())

    ## Valiate that setting the cache to the same matrix will not reset the inverse cache
    cache$set(x)
    checkEquals(xinv, cache$getinv())

    ## Ensure that we can override the set matrix with a new one
    cache$set(imatrix)
	checkEquals(imatrix, cache$get())

    ## Ensure that identiy cache is cleared on a set
    checkEquals(NULL, cache$getinv())

    ## Validate that makeCacheMatrix initialized with a matrix sets the value properly
    cache = makeCacheMatrix(x)
    checkEquals(x, cache$get())
}

test.cacheSolve <- function() {
    # Bad test - it doesn't actually validate when the cache is used.
    # TODO: Need to think about the best way to do something like this in R and RUnit
    # Thoughts: time the function with a big matrix and ensure that speed is up
    # Thoughts: See if I can capture the print/stdout stream so that I can test with messages
    cache <- makeCacheMatrix(x)
	checkEquals(xinv, cacheSolve(cache))

    # try it again - we don't really know if it's the cache, but it should work - need a real test
    # Currently just eyeballing that both messages are visible when the test runs
	checkEquals(xinv, cacheSolve(cache))
}

