##This program helps to get the inverse of the matrix.
##First the special matrix object is created to cache the inverse of matrix.
##The the inverse of the matrix is calculated by the second function.
##If the inverse is calculated already or the matrix is not changes then the second function should retrive the value.


##The first function, makeCacheMatrix creates a special matrix object, which is really a list containing a function to
##1.set the value of the special matrix object
##2.get the value of the special matrix object
##3.set the value of the inverse matrix
##4.get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}
##calculates the inverse matrix of the special "matrix" object created with the above function.
##It first checks if the inverse is already calculated if yes then takes it from the cache.
cacheSolve <- function(x=matrix(), ...) {
  ## Return a matrix that is the inverse of 'x'
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}