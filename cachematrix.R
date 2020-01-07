## Below are two functions. The first function creates a special matrix
## object that can cache it's inverse. The second function computes the 
## special matrix created by the first function. 

## This first function creates a special matrix
## object that can cache it's inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<-y
                inv <<- NULL
}
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse)
}

## This function computes the inverse of the special
## matrix created by the first function and returns a

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
