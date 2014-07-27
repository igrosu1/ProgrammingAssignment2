#makecachematrix
#create 'matrix' object that can cache its inverse

makecachematrix <- function(x = matrix()) {
i <- NULL
set <- function(y) {
	x <<- y
	i <<- NULL
}
get <- function () x
setinverse <- function(inverse) i <<- inverse
getinverse <- function() i
list (set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

#cacheSolve
#computes the inverse of the special "matrix" object returned by makeCacheMatrix.
#if the inverse has been calculated, then retrieve inverse from cache. 
cacheSolve <- function(x, ...) {
i <- x$getinverse()
#if inverse has already been calculated, retrieve cache data. 
if(!is.null(i)) {
	message("getting cached data")
	return(i)
}
#if not,calculate inverse of matrix. 
data <- x$get()
i <- solve(data)
x$setinverse(i)
i
}
