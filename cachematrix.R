## Below are two functions used to create an object that 
## a matrix and caches its inversion

## The first function, makeCacheMatrix creates a vector 
## containing functions to 
## 1) set matrix value
## 2) get matrix value
## 3) get matrix inversion
## 4) set matrix inversion

makeCacheMatrix <- function(x = matrix()) {
    inver <- NULL
    set <- function(y) {
        x <<- y
        inver <<- NULL
    }
    get <- function() x
    setinversion <- function(inversion) inver <<- inversion
    getinversion <- function() inver
    list(set = set, get = get, setinversion = setinversion, getinversion = getinversion)
    
}


## The second function check whether the inversion has 
## already been calculated. If so, it returns the cached
## inversion and skips the calculation. Otherwise, it
## calculates the inversion and set the value in the cache

cacheSolve <- function(x, ...) {
    inver<- x$getinversion()
    if(!is.null(inver)) {
        message("getting cached data")
        return(inver)
    }
    data <- x$get()
    inver <- solve(data, ...)
    x$setinversion(inver)
    ## Return a matrix that is the inverse of 'x'
    inver
    
        
}

