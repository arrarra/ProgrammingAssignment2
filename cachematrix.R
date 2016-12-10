# cachematrix.R defines special "matrices" that are able to cache their inverses.
# This may save a lot of time if the inverse is used often.
# The implementation consists of two functions `makeCacheMatrix` and `cacheSolve`.

# Function `makeCacheMatrix`
# Description:
#       Function `makeCacheMatrix` creates a special "matrix" object,
#       which is represented as a list of four functions, `set`, `get`,
#       `setinverse`, and `getinverse`.
#       Functions `set` and `get` are used for storing and retrieving
#       the contents of the matrix. Functions `setinverse` and `getinverse`
#       are used for storing and retrieving the inverse of the matrix.
#       The inverse is initially NULL. Call function `cacheSolve` to compute and store it.
# Arguments:
#       x       the contents of the special "matrix"
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function (y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function () x
        setinverse <- function (inv) inverse <<- inv
        getinverse <- function () inverse
        list (set = set, get = get, getinverse = getinverse, setinverse = setinverse)
}

# Function `cacheSolve`
# Description:
#       Function `cacheSolve` computes the inverse of the special
#       "matrix" returned by `makeCacheMatrix` above. If the inverse has
#       already been calculated (and the matrix has not changed), then
#       `cacheSolve` retrieves the inverse from the cache.
#       We assume that the matrix is always invertible.
# Arguments:
#       x       a special "matrix" returned by `makeCacheMatrix`
#       ...     further arguments that can be passed to function `solve`
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
