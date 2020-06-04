## The following describes both the functions makeCacheMatrix and CacheSolve
## makeCacheMatrix - This function creates a matrix based on the function argument. 
## It has 4 functions inside it's body - set, get, setinverse, getinverse
## set - setting a new values to an existing matrix
## get - getting value of an existing matrix
## setinverse - setting a inverse for the matrix
## getinverse - getting a inverse of the matrix

makeCacheMatrix <- function(x = matrix( )) {
        inverse<-NULL
        set <- function(y) {
                x<<-y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(i) inverse <<- i
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
        
}


## cacheSolve - checking if the inverse if already in the cache or not. IF yes, then getting inverse from cache else calculating it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        ?solve
        x$setinverse(inverse)
        inverse
        
}
?as.matrix
##---------Example 1
mat<-matrix(c(-1,1,1.5,-1),2,2)
m<-makeCacheMatrix(mat)
m$get()
m$getinverse() 
cacheSolve(m)
m$getinverse()

##---------Example 2
m$get()
mat2<-matrix(c(1,-5,0,0,1,0,0,0,1),3,3)
m$set(mat2)
m$get()
cacheSolve(m)
m$getinverse()


##---------Example 3
mat3<-matrix(c(1,0,0,0,1,4,0,0,1),3,3)
m$setinverse(mat3)
m$getinverse()
m$get()
cacheSolve(m)



