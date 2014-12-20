## PA2 -- Caching the Inverse of a Matrix
## Below are two functions that are used to create a special object that stores a matrix 
## and cache's its inverse.

## 1st function: makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## The object saves the matrix to variable x and its inverse to variable s in scope.
## The function returns object, which is really a list containing a fucntion to:
## 1. set: sets matrix and resets cached inverse
## 2. get: returns matrix
## 3. setinverse: saves solve value
## 4. getinverse: returns cached inverse value

makeCacheMatrix <- function(x = matrix()) {
        ## set the value of the matrix and reset cached inverse
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        
        ## get the value of the matrix
        get <- function() x
        
        ## set the inverse of the matrix
        setinverse <- function(solve) s <<- solve
        
        ## get the inverse of the matrix
        getinverse <- function() s
        
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}


## 2nd function: cacheSolve computes the inverse of the special "martix" returned by makeCacheMatrix above.
## It takes the object of that type as an argument 'x', checks if the inverse has already been calculated 
## (and the matrix has not changed), then the cacheSolve retrieves the inverse from cache. If the inverse 
## is not calculated, then this function calculates the inverse for the matrix and saves it into 'x' cache
## using method 'setinverse' and returns the result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## get the inverse of the matrix
        s <- x$getinverse()
        
        ## check if there is the inverse
        if(!is.null(s)) {
                message("getting cached inverse matrix")
                return(s)
        }
        
        ## if not, then calculate the inverse of the matrix
        data <- x$get()
        s <- solve(data, ...)
        
        ## set the inverse of the matrix
        x$setinverse(s)
        s
}
