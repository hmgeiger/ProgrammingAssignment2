#These functions are to cache the inverse of a matrix rather than computing it repeatedly, in order to save time and computational resources.

#For the first function makeCacheMatrix, we are going to set up a list of functions to set up the matrix and to have the function available to invert it.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inverse <<- solve
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


#For the second function cacheSolve, we want to return a matrix that is the inverse of what is input. However, first we check the cache to see if we already have the inverse available so we don't need to calculate it again in that case.
#The first time it runs/if there is no inverse in the cache, it will calculate the inverse using  the solve function. After that, it will retrieve it from the cache.

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data,...)
        x$setinverse(inverse)
        inverse
}

#Test by checking runtime for a really big matrix. Should be much less when running cacheSolve for the second time since then you are just retrieving it from the cache. It will also give the "getting cached data" message the second time.

#matrix_to_invert <- matrix(rnorm(1000000),ncol=1000,nrow=1000)

#op <- options(digits.secs = 2)

#matrix_to_input_to_cacheSolve <- makeCacheMatrix(matrix_to_invert)

#Sys.time();matrix_inverted <- cacheSolve(matrix_to_input_to_cacheSolve);Sys.time()

#Sys.time();matrix_inverted_2 <- cacheSolve(matrix_to_input_to_cacheSolve);Sys.time()
