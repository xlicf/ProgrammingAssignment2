# Function Descriptions: =======================================================
# These two functions are used to create a special object that stores
# a matrix and caches its inverse matrix.



# This file is written by Leo Xuan Li @ 2016-08-12
# for the assignmnet in Coresera Data Science Course 2


# Function: `makeCacheMatrix` creates a special "vector", which is
# really a list containing a function to

# 1.  set(): set the value of the matrix
# 2.  get(): get the value of the matrix
# 3.  setinv(): set the value of the inverse matrix
# 4.  getinv(): get the value of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)


}


# Function: 'cacheSolve' calculates the inverse of the special "vector"
# created with the `makeCacheMatrix` function.

# Step1 It first checks to see if the inverse matrix has already been
#       calculated.
# Step2 If so, it `get`s the inverse matrix from the cache and skips
#       the computation.
# Step3 Otherwise, it calculates the inverse matrix of the data and sets
#       the value of the inverse matrix n in the cache via the `setinv`
# function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mx <- x$get()
        inv <- solve(mx, ...)
        x$setinv(mx)
        inv
}
