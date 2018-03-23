## This function creates an object "matrix" which can cache its inverse
makeCacheMatrix <- function(x = matrix()) { ##Argument is defined with default mode of matrix
inver <- NULL ## inver is initialized as NULL, this will contain the marix inverse later on
setM <- function(y) { ##set function is defined to assign new
x<<-y ## value of x (matrix) to an environment different from the current environment
invM<<-NULL ## default invM to NULL in case there is a new matrix
}
getM <-function() x ## get function is defined, this will return value of the matrix (x) argument
setinverse <- function(inverse) invM <<- inverse ## thi s will assign value of invM in parent environment
getinverse <-function() invM ##get value of invM
list(setM=setM, geMt=getM, setinverse = setinverse, getinverse = getinverse)

}



## This computes the inverse of the matrix returned by the makeCacheMatrix function created above and checks if the inverse matrix has any value. If it is empty, it gets the matrix and sets the inverse matrix using the solve function
##If the matrix from previous code has value, it returns "Getting cached data" and the cached object
cacheSolve <- function(x, ...) {
      invM <- x$getinverse() ## the list created earlier is useful in this way to refer
if(!is.null(invM)) {
message("Getting cached data")
return(invM)
}
dataM <- x$getM()
invM <-solve(dataM, ...)
x$setinverse(invM)
return(invM)
}
