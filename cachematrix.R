## computing matrix inversion can be time-consuming
## so you don't want to compute it repeatedly 
## here are two functions to cache the inverse of a matrix

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix<- function(x = matrix()){
        m <- NULL
        set <- function(y) {         ## to change the original matrix
                x <<- y                              
                m <<- NULL           ## reset all 'm' to'NULL', so 'getinverse' will also be 'Null' 
        }
        get <- function() x                  ## get original matrix
        setinverse <- function(inverse) m <<- inverse    ##cache matrix inversion
        getinverse <- function() m                       ##get matrix inversion
        list(set = set, get = get,                       ##return a list of four functions
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by 'makeCacheMatrix' above

cacheSolve <- function(x, ...) {
        m <- x$getinverse()                             ##get cached matrix inversion
        if(!is.null(m)) {                               ##if the inversion has already been calculated
                message("getting cached data")
                return(m)                               ##return the inversion from the cache
        }
        data <- x$get()                                 ##else, get the original matrix
        m <- solve(data, ...)                           ##compute the matrix inversion
        x$setinverse(m)                                 ##cache the inversion via the 'setinverse' function
        m          
}
        ## Return a matrix that is the inverse of 'x'

