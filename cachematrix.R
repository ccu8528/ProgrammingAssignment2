#Caching the inverse of a Matrix
#The calculation of the investment of a matrix is very expensive
# Instead of repeatedly calculating the inverse of the matrix is cached.
#the makeCacheMatrix function initially creates a list that contains a function for(1)set the value of the matrix
# (2)Get the value of the matrix,(3)Set the value of inverse of the matrix
# (4)Get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inver <<- inverse
  getinverse <- function() inver
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

#The cacheSolve function returns the inverse of the matrix
#(1) check if your inverse is already calculated
#(2) then if so, the result is obtained and the calculation is omitted,
#if this does not happen, the inverse is calculated and the value is set in the cache by means of the reverse set function.
#this function assumes that the matrix is always invertible

cacheSolve <- function(x, ...) {
  inver <- x$getinverse()
  if(!is.null(inver)) {
    message("getting cached data.")
    return(inver)
  }
  data <- x$get()
  inver <- solve(data)
  x$setinverse(inver)
  inver
}

## Evidence test

## > x = rbind(c(1, -1/3), c(-1/3, 1))
## > m1 = makeCacheMatrix(x)
## > m1$get()
##          [,1]       [,2]
##[1,]  1.0000000 -0.3333333
##[2,] -0.3333333  1.0000000

## > cacheSolve(m1)
##             [,1]  [,2]
##[1,] 1.125 0.375
##[2,] 0.375 1.125
