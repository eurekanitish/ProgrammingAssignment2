## Put comments here that give an overall description of what your
## functions do
## Overall Description :- The following functions are developed 
## to cache potentially time consuming computation like solving 
## inverse of any invertible matrix. It takes invertible matrix 
## as a argument for the set() function returned by makeCacheMatrix()
## and creates a special "matrix". cacheSolve() solves for the inverse 
## and returns it for the matrix which has not been called before, otherwise
## it just returns value of the inverse from its cache.

## Desription of Functions :- 
## makeCacheMatrix creares a special "matrix"
## which returns a list containing a function to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse



## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## Write a short comment describing this function
## cacheSolve function calculates the inverse of the special
## "matrix" created with the above function. If inverse is computed
## before it returns inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse and sets the value of the inverse 
## via the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
