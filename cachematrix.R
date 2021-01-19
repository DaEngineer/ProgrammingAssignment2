
## creates a class-esc structure with the nternal x variable
##  initialized to <insMatDataHere> when makeCacheMatrix(<insMatDataHere>)
##  is called; eg
##  A <- makeCacheMatrix(<insMatDataHere>)
##  x is returned when A$get
##  inv is returned if stored A$getInv

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL

    set <- function(A){
        x <<- A
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(solvedInv) inv <<- solvedInv
    getInv <- function() inv
    list(set = set,
         get = get,
         setInv = setInv,
         getInv = getInv)
}

## Calculates and caches the inverse of a matrix if it has not already been
##  calculated and stored in the class-esc structure created by the
##  makeCacheMatrix function; eg:
##  cacheSolve(A)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInv()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInv(inv)
    inv
}
