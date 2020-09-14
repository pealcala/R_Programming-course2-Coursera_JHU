##Funcion para crear un vector especial, que contiene una lista con una funion para: #########
## establecer el valor del vector
## obtener el valor del vector
## establecer el valor de la media
## obtener el valor de la media

makeCacheMatrix <- function(x = matrix()) {
        j <- NULL
        set <- function(y){
                x <<- y
                j <<- NULL
        }
        get <- function()x
        setInverse <- function(inverse) j <<- inverse
        getInverse <- function() j 
        list(set = set, get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
}


## La siguiente función calcula la media del "vector" especial creado con la función anterior. 
## Sin embargo, primero verifica si la media ya se ha calculado. Si es así, obtiene la media 
##de la caché y omite el cálculo. De lo contrario, calcula la media de los datos y establece 
##el valor de la media en la caché mediante la función setmean.


cacheSolve <- function(x, ...) {
        j <- x$getInverse()
        if(!is.null(j)){
                message("datos en caché")
                return(j)
        }
        mat <- x$get()
        j <- solve(mat,...)
        x$setInverse(j)
        j
}