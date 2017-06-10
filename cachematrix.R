## Put comments here that give an overall description of what your
## functions do

### The first function, `makeCacheMatrix` creates a special "matrix", which is really a matrix containing a function to

## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse of the matrix
## 4.  get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) 
{
    testinv = NULL
    set <- function(y)
    {
        x <<- y
        testinv <<- NULL
    }
    get    <- function() x
    setinv <- function(inverse)  testinv <<- inverse
    getinv <- function() testinv
    list(get = get, set = set, setinv = setinv, getinv = getinv)

}


## The following function calculates the inverse of the special "matrix" created earlier with "makeCacheMatrix" function
## First checks to see if calculation of the inverse already exists or not. If YES, it `get`s the inverse from the cache and 
## skips further computation. If NO, it calculates the inverse of the given matrix data and sets the value of the inverse 
## in the cache through the "setinv" function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        testinv <- x$getinv()
        if(!is.null(testinv)){
          message("getting cached data")
          return(testinv)
        }
        data <- x$get()
        testinv <- solve(data, ...)
        x$setinv(testinv)
        print(testinv)
        
}
