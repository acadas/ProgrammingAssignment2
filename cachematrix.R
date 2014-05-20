## Caching the Inverse of a Matrix

## Matrix inversion is usually a costly computation.
## There may be some benefit to caching the inverse of
## a matrix rather than compute it repeatedly.


makeCacheMatrix <- function(x=matrix()) {
## Creates a list of functions that
## can cache the inverse of a matrix.
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) m <<-inverse
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}

cacheSolve <- function(x, ...) {
## Computes the inverse of the matrix returned
## by makeCacheMatrix(), unless the inverse has
## already been calculated, in which case
## it retrieves it from the cache.
    m <- x$getInverse()
    if ( ! is.null(m)) {
        print("getting cached data")
        return(m)
    }
    m <- solve(x$get())
    x$setInverse(m)
    m
}


## Test cases - uncomment to run
## With a big THANKS to Fu Sheng Wang and his excellent forum post!

a <- makeCacheMatrix(matrix(1:4,2))
a$get()
a$getInverse()
a$set(matrix(5:8,2))
a$get()
cacheSolve(a)
cacheSolve(a)
a$getInverse()
b = a$getInverse()
a$get() %*% b
