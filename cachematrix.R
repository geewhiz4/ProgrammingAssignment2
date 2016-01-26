## The following two functions create a special object that stores a matrix and caches its inverse


## The first function creates a cache for a matrix. The cache is a list containing a function that:
## 1. sets the value of the matrix
## 2. gets the value of the matrix
## 3. sets the value of the inverse of that matrix
## 4. gets the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function calculates the inverse of the special "matrix" created with the above function.
## It first checks to see if the inverse matrix has already been calculated. If so, it gets the 
## cached inverse matrix and skips the comoputation. Otherwise, it calculates the inverse matrix
## and sets the value of the inverse matrix in the cache via the setmatrix function.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {                       ##checks to see if inverse of matrix exists in the cache
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)                   ##calculates the inverse of the matrix
    x$setinverse(inv)
    inv
}