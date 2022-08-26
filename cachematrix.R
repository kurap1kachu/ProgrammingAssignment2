## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
                inv <- NULL         #initializing inverse as NULL
                set <- function(y) {
                        x <<- y
                        inv <<- NULL
                }
                get <- function() x #function to get matrix x
                setinverse <- function(inverse) inv <<- inverse
                getinverse <- function() inv
                list(set = set, get = get,
                     setinverse = setinverse,
                     getinverse = getinverse)
        }


## Write a short comment describing this function

cacheinverse <- function(x, ...) {  #gets cache data
        inv <- x$getinverse()
        if(!is.null(inv)) {   #checks if inverse is NULL
                message("getting cached data")
                return(inv)    #returns inverse value
        }
        data <- x$get()
        inv <- solve(data, ...)  #calculate inverse value
        x$setinverse(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}
