## These are two functions that allow use of 'special' matrix object.
## This special matrix object cache inverse of the underlying matrix
## This is code for Programming Assignment number 02 in John Hopkins' R Programming course on Coursea
## author: Lukasz Gadomski

## This function create 'special' matrix object.
## It stores the input matix and the inverse of it. After the object is created the inverse is set to NULL.
## To calcule and cache inverse you have to call 'cacheSolve' function.
## To fetch underlying matrix object call its 'get' method.
## To fetch inverse matrix object call its 'getInv' method (returns inverse of x or NULL).
## To set inverse call 'setInv'.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <- NULL
    }
    get <- function() x ## return the matrix
    setInv <- function(solvemc) inv <<- solvemc
    getInv <- function() inv
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}



## Returns a matrix that is the inverse of 'x' where x is a 'special' matrix created using function 'makeCacheMatrix'.
## If 'x' does not contain cached value then it will be calculated and returned.
## If 'x' contains cached value it will be returned without additional calculations. 
## WARNING: It will not refresh cached value if the underlying matrix has changed. You have to create a new object using 'makeCacheMatrix' function.
cacheSolve <- function(x, ...) {
    inv <- x$getInv()
    if(!is.null(inv)) { ## if cached inverse is not null then return it
        message("returning cached inverse matrix")
        return(inv)
    }
    inv <- solve(x$get(), ...)
    x$setInv(inv)
    inv
}
