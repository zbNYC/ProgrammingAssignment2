## The following is a pair of functions that cache
## the inverse of a matrix

## This function creates a special "matrix" object
## that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
		m_inverse <- NULL
		set <- function(y) {
		x <<- y
		m_inverse <- NULL
		}
		
		get <- function() x
		setinverse <- function(inv) m_inverse <<- inv
		getinverse <- function() m_inverse
		list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix"
## returned by the makeCacheMatrix function. For the inverse of 
## the unchanged matrix that has already been calculated,
## this function retrieves the inverse from the cache rather than
## computing inverse.

cacheSolve <- function(x, ...) {
m_inverse <- x$getinverse()                      
if(!is.null(m_inverse)) {                     
message("getting cached data")
return(m_inverse)                     
}                                     
data <- x$get()                       
m_inverse <- solve(data, ...)                  
x$setinverse(m_inverse)                          
m_inverse               	 
}

##Usage Example
##> m <- makeCacheMatrix(matrix(1:4, nrow = 2, ncol = 2))
##> m$get()
##     [,1] [,2]
##[1,]    1    3
##[2,]    2    4
#####Calculate inverse
##> cacheSolve (m)
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
####Get inverse from cache 
##> cacheSolve (m)
##getting cached data
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##> 