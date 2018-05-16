## This file provides a pair of functions that create a special "matrix"
## object, which provides the service of caching the inverse

## This function creates a special "matrix" which is really a list
## containing functions to:
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the inverse value of the matrix
## 4. Get the inverse value of the matrix

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() {
		x
	}
	setinverse <- function(inverse) {
		inv <<- inverse
	}
	getinverse <- function() {
		inv
	}
	list(set=set,
		 get=get,
		 setinverse=setinverse,
		 getinverse=getinverse)
}


## This function will solve the inverse of the special "matrix" 
## created via makeCacheMatrix, caching the result.
cacheSolve <- function(x, ...) {
	inv <- x$getinverse()
	if(is.null(inv)) {
		m <- x$get()
		inv <- solve(m, ...)
		x$setinverse(inv)
	}
	inv
}

