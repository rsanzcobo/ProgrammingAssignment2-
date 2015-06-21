##  Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly 
##  This R file contains a pair of functions that cache the inverse of a matrix.
##  makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##  cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.


#####################################################
#   Example to test execise.
#
#   source('cachematrix.R')
#
#   m <- makeCacheMatrix(matrix(c(-1, -2, 1, 1), 2,2))
#
#   cacheSolve(m)
#        [,1] [,2]
#   [1,]    1   -1
#   [2,]    2   -1
#   
#   cacheSolve(m)
#
#   getting cached data
#         [,1] [,2]
#   [1,]    1   -1
#   [2,]    2   -1
#
#  I apologize because I have written de comments in my mother language, Spanish.
#
#######################################################

##  This function creates a special "matrix" object that can cache its inverse.
##  This function was writen by a spanish classmate, the comments are written in Spanish. Sorry.

makeCacheMatrix <- function(x = matrix()) {
  
  
  #Inicializamos la variable i
  i <- NULL
  
  # Establece la Matriz a la variable global x
  
  set <- function(y) {
    
        x <<- y #Asigna la Matriz Y a la variable global X
    
        i <<- NULL #Inicializo la variable global i a NULL
    
  } # End set
  
  # Devuelve el valor de X. Recordar que las funciones devuelven el valor de la última sentencia.
  
  get <- function() x 
  
  #End get
  
  # Asigna al objeto i el valor inverse.
  
  setInverse <- function(inverse) i<<- inverse
  
  #End setInverse
  
  # Devuelve el valor de i
  
  getInverse <- function() i
  
  #End getInverse
  
  # Almacena las funciones como objetos de la función para poder hacer referencia a las mismas nombre_función$subfunción
  
  list(set = set, get = get,
       setinverse = setInverse,
       getinverse = getInverse)
  
} # End makeCacheMatrix


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.
## For this assignment, assume that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
  
  
  # Asigmamos la matriz inversa calculada en el objeto X

  i <- x$getinverse()
  
  # Comprobamos si el objeto i tiene la matriz inversa o no.

  if(!is.null(i)) {
    
        # Tenemos la matriz inversa e informamos al usuario que la estamos cogiendo
    
        message("getting cached data")
    
        #Devolvemos la matriz inversa y salimos de la funciòn para ahorrarnos el cálculo de la matriz inversa
    
        return(i)
    
  }# End if
  
  # Asignamos a la variable data la matriz de la que queremos calcular su matriz inversa

  data <- x$get()
  
  # Calculamos
  
  i <- solve(data, ...)
  
  # Signamos al objeto x la matriz inversa
  
  x$setinverse(i)
  
  #Devolvemos la matriz inversa.
  
  i
  
} # End cacheSolve()
