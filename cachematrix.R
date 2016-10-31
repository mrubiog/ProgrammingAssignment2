## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    m_inv <- NULL
    set <- function(y) {
        x <<- y
        m_inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverseMatrix) m_inv <<- inverseMatrix
    getInverse <- function() m_inv
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m_inv <- x$getInverse()
    if (!is.null(m_inv)) {
        return(m_inv)
    }
    mat <- x$get()
    m_inv <- solve(mat, ...)
    x$setInverse(m_inv)
    m_inv
}
