## makeCacheMatrix offers a wrapper for the matrix 
## to cache the value of its data's inverse.
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function(){
        x
    }
    setinverse <- function(localInverse){
        inverse <<- localInverse    
    } 
    getinverse <- function(){ 
        inverse
    }
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve calculates the data's inverse. If it has calculated it before, 
# it avoids calculating it again and fetches it from the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data)
    x$setinverse(inverse)
    inverse
}
