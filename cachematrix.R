## The following describes both the functions makeCacheMatrix and CacheSolve
## makeCacheMatrix - This function creates a matrix based on the function argument. 
## It has 4 functions inside it's body - set, get, settranspose, gettranspose
## set - setting a new values to an existing matrix
## get - getting value of an existing matrix
## settranspose - setting a transpose for the matrix
## gettranspose - getting a transpose of the matrix

makeCacheMatrix <- function(x = matrix()) {
        transpose<-NULL
        set <- function(y) {
                x <<- y
                transpose <<- NULL
        }
        get <- function() as.matrix(x)
        settranspose <- function(t) transpose <<- t
        gettranspose <- function() transpose
        list(set = set, get = get,
             settranspose = settranspose,
             gettranspose = gettranspose)
        
        
}


## cacheSolve - checking if the transpose if already in the cache or not. IF yes, then getting transpose from cache else calculating it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        transpose <- x$gettranspose()
        if(!is.null(transpose)) {
                message("getting cached data")
                return(transpose)
        }
        data <- x$get()
        transpose <- t(data, ...)
        x$settranspose(transpose)
        transpose
        
}

m<-makeCacheMatrix(1:8)
m$get()
m$gettranspose() 
m$settranspose(as.matrix(10:19))
m$set(10:40)
m$get()
cacheSolve(m)
m$gettranspose()


