## This function do a matrix and transversal matrix
## his function creates a special "matrix" object 
##that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inver<-NULL #declarar variable
  set<- function(y){#dar un nuevo valor a la matriz existente
    x<<-y
    inver<<-NULL
  }
  get<-function()x#Devolver el valor de la matriz existente
  setinver <- function(inverse) inver <<- inverse
  getinver <- function()inver#devuelve el valor de la inversa
  list(set = set, get = get,
       setinver = setinver,
       getinver = getinver)
}

## This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above.
##If the inverse has already been calculated 
##(and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  inver <- x$getinver()
  if(!is.null(inver)){
    message("getting cached data")
    return(inver)
  }
  data <- x$get()
  inver <- solve(data, ...)
  x$setinver(inver)
  inver
}