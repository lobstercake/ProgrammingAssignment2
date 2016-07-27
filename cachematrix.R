## cacheSolve will compute the inverse of the matrix stored in makeCacheMatrix.
## If the matrix in makeCacheMatrix has already been computed, cacheSolve will retrieve its inverse from makeCacheMatrix without having to recompute its value.

## creates a matrix that will cache its inverse. z will be the inverted matrix, x is the input matrix.

makeCacheMatrix <- function(x = matrix()) {
        z = NULL
        set = function(y){
                x <<- y
                z <<- NULL
        }
        get = function() x
        setinv = function(solve) z <<- inverse
        getinv = function() z
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}



## computes inverse of matrix in makeCacheMatrix. If the inverse was already computed, the cached value (stored in makeCacheMatrix) will be retrieved

cacheSolve <- function(x, ...) {
        z = x$getinv()
        if(!is.null(z)){
                message("getting cached matrix inverse")
                return(z)
        }
        data = x$get()
        z = solve(data, ...)
        x$setinv(z)
        z
}
