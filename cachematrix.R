## The functions in this file are able to cache a matrix and it's inverse
## and retrieve this cached matrix

## This method takes a matrix and stores this matrix in a matrix object,
## this method is also able to retrieve the matrix from cache and
## is able to store and retrieve the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    invMat <- NULL
    set <- function(y) {
        x <<- y
        invMat <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) invMat <<- inv
    getInverse <- function() invMat
    list(set = set, 
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This method gets the inverse of a matrix
## If the inverse of the matrix is stored, the cached matrix will be returned
## If the inverse is not stored, it will be calculated and stored for later use
cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("getting cached matrix")
        return(inverse)
    }
    matrix <- x$get()
    matrixInv <- solve(matrix)
    x$setInverse(matrixInv)
    matrixInv
}

## Simple test example 
m1 <- matrix(c(3, 3.2, 3.5, 3.6), 2, 2)

## Make a matrix object out of the matrix
matrixObject <- makeCacheMatrix(m1)

## Calculating the inverse of the matrix as it is not cached yet
cacheSolve(matrixObject)

## Retrieving matrix from cache; this should yield the text 'getting cached matrix',
## as the result should have been stored in the previous function call
cacheSolve(matrixObject)

