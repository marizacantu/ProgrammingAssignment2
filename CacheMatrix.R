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
#Test
example <- makeCacheMatrix(matrix(1:4,2))
example$get()
example$getInverse()
example$set(matrix(5:8,2))
example$get()
cacheSolve(example)
cacheSolve(example)
example$getInverse()

#solution
#Test
> example <- makeCacheMatrix(matrix(1:4,2))
> example$get()
     [,1] [,2]
[1,]    1    3
[2,]    2    4
> example$getInverse()
NULL
> example$set(matrix(5:8,2))
> example$get()
     [,1] [,2]
[1,]    5    7
[2,]    6    8
> cacheSolve(example)
     [,1] [,2]
[1,]   -4  3.5
[2,]    3 -2.5
> cacheSolve(example)
[1] "getting cached data"
     [,1] [,2]
[1,]   -4  3.5
[2,]    3 -2.5
> example$getInverse()
     [,1] [,2]
[1,]   -4  3.5
[2,]    3 -2.5
> 


