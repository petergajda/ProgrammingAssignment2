## The following script contains two functions namely makeChacheMatrix and 
## cacheSolve. The second funtion cacheSolve calculates the inverse of a matrix.
## However, in order to prevent the function cacheSolve to recalculate the 
## inverse if it was calculated before and instead load it from the cache, the
## first function makeCacheMatrix stores the information if the output was
## calculated before or not.

## The function makeChacheMatrix builds a set of four functions and returns it 
## within a list. After initializing the objects x and s, the behaviour of the 
## objects is defined in set(). With the <<- assignment operator we assign the 
## value to x and s in the parent environment - any value of s gets cleared and
## is set to NULL. After that, get(), setsolve() and getsolve() are defined and
## the output is a list containing set(), get(), setsolve() and getsolve().

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## The following function calculates the inverse of a matrix which was created
## by the function above. Since makeCacheMatrix sets the cached inversed matrix
## to Null, every time when s is not equal Null, the function returns s from the
## cache. Otherwise CacheSolve gets the matrix from the input object, calculates
## the inverse with the solve function and sets the inverse to the input object
## and returns the inverse matrix.

cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}




################### Testing the program ####################


testmatrixA <- matrix(rnorm(25), nrow=5, ncol=5) # Defining testmatrixA
testmatrixB <- matrix(rnorm(36), nrow=6, ncol=6) # Defining testmatrixB


# TEST 1
myMatrix <- makeCacheMatrix (testmatrixA) # setting testmatrixA

cacheSolve(myMatrix) # 1st run: Inverse of testmatrixA (not from cache)
cacheSolve(myMatrix) # 2nd run: Inverse of testmatrixA (loaded from cache)


# TEST 2
myMatrix <- makeCacheMatrix (testmatrixB) # setting testmatrixB

cacheSolve(myMatrix) # 1st run: Inverse of testmatrixA (not from cache)
cacheSolve(myMatrix) # 2nd run: Inverse of testmatrixA (loaded from cache)


# TEST 3
myMatrix <- makeCacheMatrix (testmatrixA) # setting testmatrixA again

cacheSolve(myMatrix) # 1st run: Inverse of testmatrixA (not from cache)
cacheSolve(myMatrix) # 2nd run: Inverse of testmatrixA (loaded from cache)

