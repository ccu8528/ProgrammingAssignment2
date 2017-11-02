# En el siguiente script se calcula la inversa de una matriz
# a trav�s de las dos funciones expresos a continuaci�n.

# La funci�n makeCacheMatrix captura una matriz x en donde realiza
# las siguientes operaciones:
# 1) Asigna el valor de la matriz
# 2) Obtiene el valor de la matriz
# 3) Asigna el valor de la inversa de la matriz
# 4) obtiene el valor de la inversa de la matriz

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# La siguiente funci�n retorna la inversa de una matr�z, para lo cual
# primero verifica que la inversa de la matriz ya haya sido calculada, despu�s
# obtiene el resultado.

# Esta funci�n asume que la matr�z siempre es invertible.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}

## Rutina de ejemplo en R:

## > x = rbind(c(1, -1/4), c(-1/4, 1))
## > m = makeCacheMatrix(x)
## > m$get()
##       [,1]  [,2]
## [1,]  1.00 -0.25
## [2,] -0.25  1.00

## > cacheSolve(m)
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667