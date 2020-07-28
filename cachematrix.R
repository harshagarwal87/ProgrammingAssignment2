## Below functions are used to calculate inverse of a
## a matrix for the first time and save it in the cache.
## From the second time inverse matrix will be retrieved
## from cache if matrix hasn't been changed.

## makeCacheMatrix will make a special vector that is 
## list of matrix object containing 4 functions

## 1) Set the value of the matrix
## 2) Get the value of the matrix
## 3) Set the value of the inverse matrix
## 4) Get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    Inversematrix <- NULL
    set <- function(y) {
      x <<- y
      Inversematrix <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) Inversematrix <<- inverse
    getInverse <- function() Inversematrix
    list(set = set, get = get,
      setInverse = setInverse,
      getInverse = getInverse)
}


## The following function calculates the inverse of the 
## special "vector" created with the above function. 
## However, it first checks to see if the inverse matrix
## has already been calculated. If so, it gets the matrix
## from the cache and skips the computation. Otherwise, 
## it calculates the inverse of the data and sets the value 
## of the inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    Inversematrix <- x$getInverse()
    if(!is.null(Inversematrix)) {
      message("getting cached data")
      return(Inversematrix)
    }
    data <- x$get()
    Inversematrix <- solve(data, ...)
    x$setInverse(Inversematrix)
    Inversematrix
}
