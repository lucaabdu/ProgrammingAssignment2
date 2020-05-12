## Along this script I will create a pair of function that can cache the inverse 
## of a matrix. This would save computing time if I need the value of the inverse
## several times. 

# The "special" matrix
## The following function creates a "special" matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
     z <- NULL
     set <- function(y) {
          x <<- y
          z <<- NULL
     }
     get <- function() x
     setinverse <- function(inverse) z <<- inverse
     getinverse <- function() z
     list( set = set, 
           get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}
# The computing function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed),
# then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
     z <- x$getinverse()
     if(!is.null(z)) {
          message("getting cached data")
          return(z)
     }
     data <- x$get()
     z <- solve(data, ...)
     x$setinverse(z) # this chaches the value in the list (aka the "special" matrix) we created before
     z
     ## Return a matrix that is the inverse of 'x'
}

# Let me test whether my functions work
m <- matrix(c(1:7,5,6),3,3)
mspec <- makeCacheMatrix(m) #transoforming the matrix in its "special" list version
cacheSolve(mspec)
