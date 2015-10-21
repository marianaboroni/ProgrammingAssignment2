### Assignment2

#################################################################################
## This function creates a special "matrix" object that can cache its inverse. ##
#################################################################################

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function() x
  setMatrix<-function(solve) inv<<- solve
  getMatrix<-function() inv
  list(set=set, get=get,
       setMatrix=setMatrix,
       getMatrix=getMatrix)
}

#################################################################################
## This function computes the inverse of the special "matrix" returned by      ##
## makeCacheMatrix above. If the inverse has already been calculated (and the  ##
## matrix has not changed), then the cachesolve should retrieve the inverse    ##
## from the cache.                                                             ##
#################################################################################

cacheSolve <- function(x=matrix(), ...) {
  inv<-x$getMatrix()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  matrix<-x$get()
  inv<-solve(matrix, ...)
  x$setMatrix(inv)
  inv
}
