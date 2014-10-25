## Title: Programming Assignment 2
## Class: R Programming
##
## Purpose: This program defines two functions: makeCacheMatrix and
## cacheSolve. These functions calculate the inverse of the matrix and 
## caches the result so that it does not need to be recalculate each time.
## 
##
## Functions: makeCacheMatrix & cacheSolve
## Author: Anna Zink
## Date Created: 10/21/2014
## 

# The function "makeCacheMatrix" defines functions
# for getting and setting the matrix and inverse matrix.
makeCacheMatrix<-function(x=matrix()) {
	m<-NULL; 
	set<-function(y) {
		x<<- y
		m<<- NULL
	}
	get<- function() x
	setinverse<-function(solve) m <<- solve
	getinverse<-function() m
	list(set=set, get=get, setinverse=setinverse,getinverse=getinverse)
}

# The function "cacheSolve" returns the inverse matrix. If the
# matrix exists in cache memory it returns it; otherwise it
# finds the inverse matrix using the solve function, caches it
# into memory and returns it. 
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached inverse matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
