## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly. Following two functions cache the inverse of a matrix.


# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        
        setinverse <- function(inverse) inv <<- inverse
        
        getinverse <- function() inv
        
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# Following function returns the inverse of the matrix. First, it checks if
# the inverse has already been computed. In that case, it gets the result and skips the
# computations. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

cacheSolve <- function(x, ...){
        
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data.")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}


## Sample run:
##> x = rbind(c(3, 1), c(4, 2))
##> x <- rbind(c(3, 1), c(4, 2))
##> m <- makeCacheMatrix(x)
##> m$get()
##[,1] [,2]
##[1,]    3    1
##[2,]    4    2
##> cacheSolve(m)
##[,1] [,2]
##[1,]    1 -0.5
##[2,]   -2  1.5
##> cacheSolve(m)
##getting cached data.
##[,1] [,2]
##[1,]    1 -0.5
##[2,]   -2  1.5