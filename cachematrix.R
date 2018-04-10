## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        n <- NULL
        set <- function(y) {
                x <<- y
                n <<- NULL
        }
        get <- function() x
        set_inverse <- function(inverse) n <<- inverse
        get_inverse <- function() n
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        n <- x$get_inverse()
        if(!is.null(n)) {
                message("getting cached data")
                return(n)
        }
        data <- x$get()
        n <- solve(data, ...)
        x$set_inverse(n)
        n
}


#test
# a <- matrix(c(4, 2, 7, 6), 2, 2)
# b <- makeCacheMatrix(a)
# cacheSolve(b)
