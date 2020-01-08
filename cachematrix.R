## Below are two functions. The first function creates a special matrix
## object that can cache it's inverse. The second function computes the 
## special matrix created by the first function. 

## This function creates a special matrix
## object that can; set and get the value of the matrix,
## and set and get the value of the inverse. 

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
## matrix object created by the first function or retrieves it. 

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
