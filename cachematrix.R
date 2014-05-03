## Matrix inversion is usually a costly computation, functions below are an approach
## to save computing time by caching the inverse of a matrix, and are based upon
## functions to cache mean of vectors provided by R.D. Peng

## 'makeCacheMatrix' creates an R object that can storage a matrix,
## its inverse and methods to deal with them.
makeCacheMatrix <- function(x = matrix()) {
    # This variable store the inverse of Matrix x
    inverseMx <- NULL
    
    # 'set' sets a new matrix to compute its inverse
    # and initialize the inverse Matrix, to be computed
    set <- function(y) {
        x <<- y
        inverseMx <<- NULL
    }

    # 'get' returns current matrix
    get <- function() x

    # This method store an inverse matrix computed
    setInverse <- function(inverse) inverseMx <<- inverse

    # This method returns an inverse matrix "cached" previously
    getInverse <- function() inverseMx
    
    # function returns a list
    list(set = set
        ,get = get
        ,setInverse = setInverse
        ,getInverse = getInverse
    )
}

## 'cacheSolve' returns inverse of an invertible matrix stored previously
## in an object created with 'makeCacheMatrix', inverse is computed only
## during first execution and meanwhile the matrix has not changed.
cacheSolve <- function(x, ...) {
    # Check if Matrix x was cached previously and return it in such case
    inverseMx <- x$getInverse()
    if(!is.null(inverseMx)) {
        message("getting cached inverse of Matrix X...")
        return(inverseMx)
    }
    
    # Computing the inverse...
    matrixX   <- x$get()
    inverseMx <- solve(matrixX, ...)

    # this call stores the inverse matrix and return it
    x$setInverse(inverseMx)
    inverseMx
}
