makeCacheMatrix <- function(x=matrix()) { ## Creates a list of functions that can cache the inverse of a matrix.
    inver <- NULL
    set <- function(y) {
        x <<- y
        inver <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inver <<-inverse
    getInverse <- function() inver
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
 
}
 
cacheSolve <- function(x, ...) {
## Computes the inverse of the matrix returned by makeCacheMatrix(), unless the inverse has
## already been calculated, in which case it retrieves it from the cache.
    inver <- x$getInverse()
    if ( ! is.null(inver)) {
        print("getting cached data")
        return(inver)
    }
    inver <- solve(x$get())
    x$setInverse(inver)
    inver
}
