## Calculating the inverse of a matrix can be time consuming. The following
## functions cache the values of a matrix and its inverse, allowing us to
## skip re-calculating for the same matrix values.


## Wrapper for matrix which caches its value and its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL

    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv

    list(
        set = set,
        get = get,
        setInverse = setInverse,
        getInverse = getInverse
    )
}


## Compute the inverse of given matrix using the `solve()` function.
## Results are cached if matrix has not changed since previous calculation.

cacheSolve <- function(x, ...) {
    # check if inverse result is already cached
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }

    # not cached, so calculate inverse
    inv <- solve( x$get() )
    x$setInverse(inv)

    inv
}
